# 0. Identification ---------------------------------------------------

# Title: Data preparation Longitudinal
# Institution: OCS
# Responsible: René Canales

# Executive Summary: This script contains the code to data preparation for analysis of cohesion in VIS-ELSOC
# Date: Sep 30, 2025

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               car,
               sjmisc, 
               here,
               sjlabelled,
               naniar,
               sjPlot,
               psych,
               dplyr)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

glimpse(elsoc_long_2016_2023)

# 3.2 Processing -----------------------------------------------------------

elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

db <- elsoc_long_2016_2023 %>% 
  select(idencuesta, 
         ola,
         muestra,
         tipo_atricion,
         ponderador_long_total, 
         segmento, 
         estrato,
         educacion = m01, 
         sexo = m0_sexo, 
         edad = m0_edad,
         ideologia =  c15,
         religion = c12_02,
         estado_civil =  m36,
         seguridad_sat = t06_01, 
         seguridad_perc = t10, 
         peleas_calle = t09_01,
         asaltos = t09_02,
         trafico_drogas = t09_03,
         barrio_ideal = t02_01,
         barrio_integracion = t02_02, 
         barrio_identidad = t02_03, 
         barrio_pertenencia = t02_04, 
         barrio_amigos = t03_01, 
         barrio_sociable = t03_02, 
         barrio_cordial = t03_03, 
         barrio_colaborador = t03_04, 
         confianza_gen = c02,
         altruismo_gen = c03,
         reunion_pub = c07_02, 
         voluntariado = c07_04,
         prestar_dinero = c07_06, 
         ayuda_trabajo = c07_08,
         conf_gobierno = c05_01, 
         conf_pp = c05_02, 
         conf_judicial = c05_05, 
         conf_congreso = c05_07, 
         firma_peticion = c08_01, 
         asiste_marcha = c08_02, 
         part_huelga = c08_03,
         opinion_rrss = c08_04, 
         voto_deber = c10_01, 
         voto_influye = c10_02, 
         voto_expresion = c10_03, 
         interes_politica = c13, 
         hablar_politica = c14_01, 
         infopolitica_medios = c14_02, 
         gobierno_firme = c18_04, 
         mandatario_fuerte = c18_05, 
         vida_disciplinar = c18_07, 
         justicia_pensiones = d02_01,
         justicia_educacion = d02_02, 
         justicia_salud = d02_03, 
         sat_democracia = c01) %>% 
  as_tibble() %>% 
  sjlabelled::drop_labels(., drop.na = FALSE)

# 3.2 Recode and transform ----

# Ola
frq(db$ola)

db <- db %>% 
  mutate(ola = case_when(ola == 1 ~ "2016",
                          ola == 2 ~ "2017",
                          ola == 3 ~ "2018",
                          ola == 4 ~ "2019",
                          ola == 5 ~ "2021",
                          ola == 6 ~ "2022",
                          ola == 7 ~ "2023"),
         ola = factor(ola, levels = c("2016",
                                        "2017",
                                        "2018",
                                        "2019",
                                        "2021",
                                        "2022",
                                        "2023")))


# Atricion y tipo muestra
frq(db$tipo_atricion)
frq(db$muestra)

# 3.2.1 Index Creation ----

# COHESIÓN HORIZONTAL

#-----Redes-----

# comportamiento_prosocial

db %>% 
  group_by(ola) %>% 
  select(reunion_pub, voluntariado) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(reunion_pub %in% c(1,2,3))),
    n_validos = sum(reunion_pub %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(voluntariado %in% c(1,2,3))),
    n_validos = sum(voluntariado %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes (una por una )
variables_a_transformar <- c("reunion_pub", "voluntariado")

db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~1 + (. - 1) * 2))

# Verificar el resultado
summary(db[, variables_a_transformar])  

db$comportamiento_prosocial <- rowMeans(db[, c("reunion_pub", "voluntariado")], na.rm = TRUE)

frq(db$comportamiento_prosocial);psych::describe(db$comportamiento_prosocial)

# ayuda_economica

db %>% 
  group_by(ola) %>% 
  select(prestar_dinero, ayuda_trabajo) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(prestar_dinero %in% c(1,2,3))),
    n_validos = sum(prestar_dinero %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(ayuda_trabajo %in% c(1,2,3))),
    n_validos = sum(ayuda_trabajo %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes
variables_a_transformar <- c("prestar_dinero", "ayuda_trabajo")

db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~1 + (. - 1) * 2))

# Verificar el resultado
summary(db[, variables_a_transformar]) 

db$ayuda_economica <- rowMeans(db[, c("prestar_dinero", "ayuda_trabajo")], na.rm = TRUE)

frq(db$ayuda_economica);psych::describe(db$ayuda_economica)

# confianza_inter

db %>% 
  group_by(ola) %>% 
  select(confianza_gen, altruismo_gen) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(confianza_gen %in% c(1,2,3))),
    n_validos = sum(confianza_gen %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(altruismo_gen %in% c(1,2,3))),
    n_validos = sum(altruismo_gen %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes
variables_a_transformar <- c("confianza_gen", "altruismo_gen")

db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~1 + (. - 1) * 2))

# Verificar el resultado
summary(db[, variables_a_transformar]) 

db$confianza_inter <- rowMeans(db[, c("confianza_gen", "altruismo_gen")], na.rm = TRUE)

frq(db$confianza_inter);psych::describe(db$confianza_inter)


# Redes Sociales
db %>% 
  group_by(ola) %>% 
  select(comportamiento_prosocial, ayuda_economica, confianza_inter) %>% 
  frq()

db$redes_sociales <- rowMeans(db[, c("comportamiento_prosocial", "ayuda_economica", "confianza_inter")], na.rm = TRUE)

frq(db$redes_sociales);psych::describe(db$redes_sociales)

#----Seguridad-----

# seguridad_sub

db %>% 
  group_by(ola) %>% 
  select(seguridad_sat, seguridad_perc) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seguridad_sat %in% c(1:5))),
    n_validos = sum(seguridad_sat %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seguridad_perc %in% c(1:5))),
    n_validos = sum(seguridad_perc %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$seguridad_sub <- rowMeans(db[, c("seguridad_sat", "seguridad_perc")], na.rm = TRUE)

frq(db$seguridad_sub);psych::describe(db$seguridad_sub)

# seguridad_obj

db %>% 
  group_by(ola) %>% 
  select(peleas_calle, asaltos, trafico_drogas) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(peleas_calle %in% c(1:5))),
    n_validos = sum(peleas_calle %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(asaltos %in% c(1:5))),
    n_validos = sum(asaltos %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(trafico_drogas %in% c(1:5))),
    n_validos = sum(trafico_drogas %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$seguridad_obj <- rowMeans(db[, c("peleas_calle", "asaltos", "trafico_drogas")], na.rm = TRUE)
db$seguridad_obj <- (round(db$seguridad_obj * 2) / 2)

frq(db$seguridad_obj);psych::describe(db$seguridad_obj)

# Seguridad Pública

db %>% 
  group_by(ola) %>% 
  select(seguridad_sub, seguridad_obj) %>% 
  frq()

db$seguridad_pub <- rowMeans(db[, c("seguridad_sub", "seguridad_obj")], na.rm = TRUE)

frq(db$seguridad_pub);psych::describe(db$seguridad_pub)

#-----Vínculos Territoriales-----

# sentido_pertenencia

db %>% 
  group_by(ola) %>% 
  select(barrio_ideal, barrio_integracion, barrio_identidad, barrio_pertenencia) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_ideal %in% c(1:5))),
    n_validos = sum(barrio_ideal %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_integracion %in% c(1:5))),
    n_validos = sum(barrio_integracion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_identidad %in% c(1:5))),
    n_validos = sum(barrio_identidad %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_pertenencia %in% c(1:5))),
    n_validos = sum(barrio_pertenencia %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$sentido_pertenencia <- rowMeans(db[, c("barrio_ideal", "barrio_integracion", "barrio_identidad", "barrio_pertenencia")], na.rm = TRUE)
db$sentido_pertenencia <- (round(db$sentido_pertenencia * 2) / 2)

frq(db$sentido_pertenencia)


# satisfaccion_barrio
db %>% 
  group_by(ola) %>% 
  select(barrio_amigos, barrio_sociable, barrio_cordial, barrio_colaborador) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_amigos %in% c(1:5))),
    n_validos = sum(barrio_amigos %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_sociable %in% c(1:5))),
    n_validos = sum(barrio_sociable %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_cordial %in% c(1:5))),
    n_validos = sum(barrio_cordial %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(barrio_colaborador %in% c(1:5))),
    n_validos = sum(barrio_colaborador %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$satisfaccion_barrio <- rowMeans(db[, c("barrio_amigos", "barrio_sociable", "barrio_cordial", "barrio_colaborador")], na.rm = TRUE)
db$satisfaccion_barrio <- (round(db$satisfaccion_barrio * 2) / 2)

frq(db$satisfaccion_barrio)

# Vínculos Territoriales

db %>% 
  group_by(ola) %>% 
  select(sentido_pertenencia, satisfaccion_barrio) %>% 
  frq()

db$vinculos_territ <- rowMeans(db[, c("sentido_pertenencia", "satisfaccion_barrio")], na.rm = TRUE)
frq(db$vinculos_territ)

# COHESIÓN VERTICAL

#-----Confianza en Instituciones Políticas-----

db %>% 
  group_by(ola) %>% 
  select(conf_gobierno, conf_congreso, conf_pp) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_gobierno %in% c(1:5))),
    n_validos = sum(conf_gobierno %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_congreso %in% c(1:5))),
    n_validos = sum(conf_congreso %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_pp %in% c(1:5))),
    n_validos = sum(conf_pp %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$conf_inst_pol <- rowMeans(db[, c("conf_gobierno", "conf_congreso", "conf_pp")], na.rm = TRUE)

frq(db$conf_inst_pol);psych::describe(db$conf_inst_pol)

#-----Participación Política-----

db %>% 
  group_by(ola) %>% 
  select(firma_peticion, asiste_marcha, part_huelga, opinion_rrss) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(firma_peticion %in% c(1:5))),
    n_validos = sum(firma_peticion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(asiste_marcha %in% c(1:5))),
    n_validos = sum(asiste_marcha %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(part_huelga %in% c(1:5))),
    n_validos = sum(part_huelga %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(opinion_rrss %in% c(1:5))),
    n_validos = sum(opinion_rrss %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$pp_politica <- rowMeans(db[, c("firma_peticion", "asiste_marcha", "part_huelga", "opinion_rrss")], na.rm = TRUE)

frq(db$pp_politica);psych::describe(db$pp_politica)

#-----Autoeficacia Política-----

db %>% 
  group_by(ola) %>% 
  select(voto_deber, voto_influye, voto_expresion) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(voto_deber %in% c(1:5))),
    n_validos = sum(voto_deber %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(voto_influye %in% c(1:5))),
    n_validos = sum(voto_influye %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(voto_expresion %in% c(1:5))),
    n_validos = sum(voto_expresion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$auto_efic <- rowMeans(db[, c("voto_deber", "voto_influye", "voto_expresion")], na.rm = TRUE)

frq(db$auto_efic);psych::describe(db$auto_efic)

#-----Interés en Política-----

db %>% 
  group_by(ola) %>% 
  select(interes_politica, hablar_politica, infopolitica_medios) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(interes_politica %in% c(1:5))),
    n_validos = sum(interes_politica %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(hablar_politica %in% c(1:5))),
    n_validos = sum(hablar_politica %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(infopolitica_medios %in% c(1:5))),
    n_validos = sum(infopolitica_medios %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$int_pol <- rowMeans(db[, c("interes_politica", "hablar_politica", "infopolitica_medios")], na.rm = TRUE)

frq(db$int_pol);psych::describe(db$int_pol)

#-----Practicas y actitudes politicas-----

db %>% 
  group_by(ola) %>% 
  select(pp_politica, auto_efic, int_pol) %>% 
  frq()

db$prac_acti_pol <- rowMeans(db[, c("pp_politica", "auto_efic", "int_pol")], na.rm = TRUE)

frq(db$prac_acti_pol);psych::describe(db$prac_acti_pol)

#-----Preferencias Autoritarias-----

db %>% 
  group_by(ola) %>% 
  select(gobierno_firme, mandatario_fuerte, vida_disciplinar) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(gobierno_firme %in% c(1:5))),
    n_validos = sum(gobierno_firme %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(mandatario_fuerte %in% c(1:5))),
    n_validos = sum(mandatario_fuerte %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(vida_disciplinar %in% c(1:5))),
    n_validos = sum(vida_disciplinar %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$pref_autor <- rowMeans(db[, c("gobierno_firme", "mandatario_fuerte", "vida_disciplinar")], na.rm = TRUE)
frq(db$pref_autor);psych::describe(db$pref_autor)

#-----Justicia Distributiva-----

db %>% 
  group_by(ola) %>% 
  select(justicia_pensiones, justicia_educacion, justicia_salud) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(justicia_pensiones %in% c(1:5))),
    n_validos = sum(justicia_pensiones %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(justicia_educacion %in% c(1:5))),
    n_validos = sum(justicia_educacion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )
db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(justicia_salud %in% c(1:5))),
    n_validos = sum(justicia_salud %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$just_distrib <- rowMeans(db[, c("justicia_pensiones", "justicia_educacion", "justicia_salud")], na.rm = TRUE)

frq(db$just_distrib);psych::describe(db$just_distrib)

###COHESIÓN HORIZONTAL###

db %>% 
  group_by(ola) %>% 
  select(seguridad_pub, vinculos_territ, redes_sociales) %>% 
  frq()

db$coh_horiz <- rowMeans(db[, c("seguridad_pub", "vinculos_territ", "redes_sociales")], na.rm = TRUE)
hist(db$coh_horiz)

psych::describeBy(db$coh_horiz, group = db$ola)

###COHESION VERTICAL###
db %>% 
  group_by(ola) %>% 
  select(conf_inst_pol, pp_politica, auto_efic, int_pol, pref_autor, just_distrib) %>% 
  frq()

db$coh_vert <- rowMeans(db[, c("conf_inst_pol", "pp_politica", "auto_efic", "int_pol", "pref_autor", "just_distrib")], na.rm = TRUE)
hist(db$coh_vert)

psych::describeBy(db$coh_vert, group = db$ola)

###COHESION GENERAL###
db %>% 
  group_by(ola) %>% 
  select(coh_horiz, coh_vert) %>% 
  frq()

db$coh_gral <- rowMeans(db[, c("coh_horiz", "coh_vert")], na.rm = TRUE)

hist(db$coh_gral)
psych::describeBy(db$coh_gral, group = db$ola)

# Label Variables

# 1. Vector
variables_recode <- colnames(db[,14:73])

# 2. Definir las nuevas etiquetas
nuevas_etiquetas <- c(
  "Bajo" = 1,
  "Medio" = 3,
  "Alto" = 5
)

# 3. Aplicar las etiquetas a todas las variables con un único bucle
for (variable in variables_recode) {
  db[[variable]] <- sjlabelled::set_labels(db[[variable]], labels = nuevas_etiquetas)
}

# 4. (Opcional) Verificar que una de las variables se haya recodificado correctamente
print(sjlabelled::get_labels(db$seguridad_sat))

# 3.4 Check BBDD

glimpse(db)

sjPlot::view_df(db,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)


# 4. Final data -----------------------------------------------------------

#  Database ID and Wave

# ==========================================
# BASE 1: Promedios por ID de encuesta
# ==========================================

db <- db %>%
  group_by(idencuesta) %>%             # Agrupar por el identificador del participante
  mutate(n_participaciones = n()) %>%  # Contar el número de filas (participaciones) por participante
  ungroup()

db <- db %>% 
  filter(n_participaciones>=3) %>% 
  select(-n_participaciones) # quedarse con casos que hayan participado >=3 veces

# Ver resultado
head(db)
dim(db)

# ==========================================
# BASE 2: Promedios por ola (sin NAs)
# ==========================================

db_promedios_ola <- db %>%
  select(-c(idencuesta, 
            muestra, 
            tipo_atricion, 
            ponderador_long_total, 
            segmento,
            estrato,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil)) %>%  # Excluir variables
  group_by(ola) %>%
  summarise(across(where(is.numeric), 
                   ~mean(.x, na.rm = TRUE),
                   .names = "{.col}"))
# Ver resultado
head(db_promedios_ola)
dim(db_promedios_ola)

# ==========================================
# Guardar las bases resultantes
# ==========================================

# En RData
save(db, file = here ("data/bases-vis-elsoc/db_long.RData"))
save(db_promedios_ola, file = here ("data/bases-vis-elsoc/db_promedios_ola.RData"))

# ==========================================
# BASE 3: Promedios por ola y tipo de muestra (sin NAs)
# ==========================================

db_promedios_ola_muestra <- db %>%
  select(-c(idencuesta, 
            tipo_atricion, 
            ponderador_long_total, 
            segmento,
            estrato,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil)) %>% 
  group_by(ola, muestra) %>%
  summarise(across(where(is.numeric), 
                   ~mean(.x, na.rm = TRUE),
                   .names = "{.col}"),
            .groups = "drop")

# Ver resultado
head(db_promedios_ola_muestra)
dim(db_promedios_ola_muestra)

# Guardar
save(db_promedios_ola_muestra, file = here ("data/bases-vis-elsoc/db_promedios_ola_muestra.RData"))

# ==============================================
# BASES: Recodificaciones Alto-Medio-Bajo (1-5)
# ==============================================

# 1. Define el vector con todas las variables que vas a modificar
todas_las_variables <- c(
  "seguridad_sub", "seguridad_obj", "seguridad_pub",
  "sentido_pertenencia", "satisfaccion_barrio", "vinculos_territ",
  "comportamiento_prosocial", "ayuda_economica", "confianza_inter", "redes_sociales",
  "conf_inst_pol",
  "pp_politica", "auto_efic", "int_pol", "prac_acti_pol",
  "pref_autor",
  "justicia_pensiones", "justicia_educacion", "justicia_salud", "just_distrib", 
  "coh_horiz", "coh_vert", "coh_gral"
)

# 2. Recodifica todas las variables de una vez
db_categ <- db %>%
  mutate(across(all_of(todas_las_variables), ~case_when(
    .x >= 3 ~ "Alto",
    .x < 3  ~ "Bajo"
  ))) %>% 
  select(1:13, all_of(todas_las_variables))

db_categ <- db_categ %>% 
  mutate_at(.vars = c(14:36), .funs = ~ as_factor(.))

# 3. (Opcional) Revisa el resultado en una de las variables
frq(db_categ$seguridad_sub)

# ==========================================
# BASE 1 RECODE: Promedios por ID de encuesta
# ==========================================

db_categ %>% 
  group_by(idencuesta) %>%             # Agrupar por el identificador del participante
  mutate(n_participaciones = n()) %>%  # Contar el número de filas (participaciones) por participante
  ungroup() %>% 
  summarise(t = sum(if_else(n_participaciones < 3, 1, 0)))# ok
  
# Ver resultado
head(db_categ)
dim(db_categ)

# ==========================================
# BASE 2 RECODE: Promedios por ola (sin NAs)
# ==========================================

db_categ_ola <- db_categ %>%
  select(-c(idencuesta,
            muestra, 
            tipo_atricion, 
            ponderador_long_total, 
            segmento,
            estrato,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil)) %>% 
  pivot_longer(
    cols = -1,
    names_to = "variable",
    values_to = "valor"
  ) %>% 
  na.omit()

db_categ_ola <- db_categ_ola %>%
  group_by(ola, variable, valor) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

db_categ_ola <- db_categ_ola %>%
  pivot_wider(
    names_from = variable,
    values_from = c(n, prop),
    values_fill = NA
  )

db_categ_ola

# ==========================================
# Guardar las bases
# ==========================================

# En RData
save(db_categ, file = here ("data/bases-vis-elsoc/db_categ.RData"))
save(db_categ_ola, file = here ("data/bases-vis-elsoc/db_categ_ola.RData"))

# ==========================================
# BASE 3 RECODE: Promedios por ola y tipo de muestra (sin NAs)
# ==========================================

db_categ_ola_muestra <- db_categ %>%
  select(-c(idencuesta,
            tipo_atricion, 
            ponderador_long_total, 
            segmento,
            estrato,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil)) %>% 
  pivot_longer(
    cols = -c(1:2),
    names_to = "variable",
    values_to = "valor"
  ) %>% 
  na.omit()

db_categ_ola_muestra <- db_categ_ola_muestra %>%
  group_by(ola, muestra, variable, valor) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

db_categ_ola_muestra <- db_categ_ola_muestra %>%
  pivot_wider(
    names_from = variable,
    values_from = c(n, prop),
    values_fill = NA
  )

db_categ_ola_muestra

# Guardar
save(db_categ_ola_muestra, file = here ("data/bases-vis-elsoc/db_categ_ola_muestra.RData"))

sjPlot::view_df(db_categ,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)

