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
  mutate(idencuesta, 
         ola,
         muestra,
         tipo_atricion,
         segmento, 
         estrato,
         educacion = m01, 
         sexo = m0_sexo, 
         edad = m0_edad,
         ideologia =  c15,
         religion = c12_02,
         estado_civil =  m36,
         nhogar1,
         m46_nhogar,
         m54, m30, m30b, m29,
         seg_seguridad_sat = t06_01, 
         seg_seguridad_perc = t10, 
         seg_peleas_calle = t09_01,
         seg_asaltos = t09_02,
         seg_trafico_drogas = t09_03,
         bar_ideal = t02_01,
         bar_integracion = t02_02, 
         bar_identidad = t02_03, 
         bar_pertenencia = t02_04, 
         bar_amigos = t03_01, 
         bar_sociable = t03_02, 
         bar_cordial = t03_03, 
         bar_colaborador = t03_04, 
         conf_inter_general = c02,
         conf_inter_altruismo = c03,
         prosoc_reunion_pub = c07_02, 
         prosoc_voluntariado = c07_04,
         ayuda_donar_dinero = c07_05,
         ayuda_prestar_dinero = c07_06, 
         ayuda_trabajo = c07_08,
         conf_inst_gobierno = c05_01, 
         conf_inst_pp = c05_02, 
         conf_judicial = c05_05, 
         conf_inst_congreso = c05_07, 
         part_firma_peticion = c08_01, 
         part_asiste_marcha = c08_02, 
         part_huelga = c08_03,
         opinion_rrss = c08_04, 
         autoef_voto_deber = c10_01, 
         autoef_voto_influye = c10_02, 
         autoef_voto_expresion = c10_03, 
         intpol_interes_politica = c13, 
         intpol_hablar_politica = c14_01, 
         intpol_infopolitica_medios = c14_02, 
         autor_gobierno_firme = c18_04, 
         autor_mandatario_fuerte = c18_05, 
         autor_vida_disciplinar = c18_07, 
         just_pensiones = d02_01,
         just_educacion = d02_02, 
         just_salud = d02_03, 
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
  select(prosoc_reunion_pub, prosoc_voluntariado) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(prosoc_reunion_pub %in% c(1,2,3))),
    n_validos = sum(prosoc_reunion_pub %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(prosoc_voluntariado %in% c(1,2,3))),
    n_validos = sum(prosoc_voluntariado %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes (una por una )
variables_a_transformar <- c("prosoc_reunion_pub", "prosoc_voluntariado")

# Primero convertir valores inválidos a NA (solo mantener 1, 2, 3)
db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~case_when(
                  . %in% c(1, 2, 3) ~ .,
                  TRUE ~ NA_real_
                )))

# Luego transformar de escala 1-2-3 a 1-3-5
db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~1 + (. - 1) * 2))

# Verificar el resultado
summary(db[, variables_a_transformar])  

db$comportamiento_prosocial <- rowMeans(db[, c("prosoc_reunion_pub", "prosoc_voluntariado")], na.rm = TRUE)

frq(db$comportamiento_prosocial)

# ayuda_economica

db %>% 
  group_by(ola) %>% 
  select(ayuda_prestar_dinero, ayuda_trabajo, ayuda_donar_dinero) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(ayuda_prestar_dinero %in% c(1,2,3))),
    n_validos = sum(ayuda_prestar_dinero %in% c(1,2,3)),
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

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(ayuda_donar_dinero %in% c(1,2,3))),
    n_validos = sum(ayuda_donar_dinero %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes
variables_a_transformar <- c("ayuda_prestar_dinero", "ayuda_trabajo", "ayuda_donar_dinero")

# Primero convertir valores inválidos a NA (solo mantener 1, 2, 3)
db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~case_when(
                  . %in% c(1, 2, 3) ~ .,
                  TRUE ~ NA_real_
                )))

# Luego transformar de escala 1-2-3 a 1-3-5
db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~1 + (. - 1) * 2))

# Verificar el resultado
summary(db[, variables_a_transformar]) 

db$ayuda_economica <- rowMeans(db[, c("ayuda_prestar_dinero", "ayuda_trabajo", "ayuda_donar_dinero")], na.rm = TRUE)

frq(db$ayuda_economica)

# confianza_inter

db %>% 
  group_by(ola) %>% 
  select(conf_inter_general, conf_inter_altruismo) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_inter_general %in% c(1,2,3))),
    n_validos = sum(conf_inter_general %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_inter_altruismo %in% c(1,2,3))),
    n_validos = sum(conf_inter_altruismo %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes - Primero recodificar orden correcto (1=1, 2=3, 3=2)
variables_a_transformar <- c("conf_inter_general", "conf_inter_altruismo")

db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~case_when(
                  . == 1 ~ 1,
                  . == 2 ~ 3,
                  . == 3 ~ 2,
                  TRUE ~ NA_real_
                )))

# Luego transformar de escala 1-2-3 a 1-3-5
db <- db %>%
  mutate(across(all_of(variables_a_transformar), 
                ~1 + (. - 1) * 2))

# Verificar el resultado
summary(db[, variables_a_transformar]) 

db$confianza_inter <- rowMeans(db[, c("conf_inter_general", "conf_inter_altruismo")], na.rm = TRUE)

frq(db$confianza_inter)


# Redes Sociales
db %>% 
  group_by(ola) %>% 
  select(comportamiento_prosocial, ayuda_economica, confianza_inter) %>% 
  frq()

db$redes_sociales <- rowMeans(db[, c("comportamiento_prosocial", "ayuda_economica", "confianza_inter")], na.rm = TRUE)

frq(db$redes_sociales)

#----Seguridad-----

# seguridad_sub

db %>% 
  group_by(ola) %>% 
  select(seg_seguridad_sat, seg_seguridad_perc) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seg_seguridad_sat %in% c(1:5))),
    n_validos = sum(seg_seguridad_sat %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seg_seguridad_perc %in% c(1:5))),
    n_validos = sum(seg_seguridad_perc %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$seguridad_sub <- rowMeans(db[, c("seg_seguridad_sat", "seg_seguridad_perc")], na.rm = TRUE)

frq(db$seguridad_sub)

# seguridad_obj

db %>% 
  group_by(ola) %>% 
  select(seg_peleas_calle, seg_asaltos, seg_trafico_drogas) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seg_peleas_calle %in% c(1:5))),
    n_validos = sum(seg_peleas_calle %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seg_asaltos %in% c(1:5))),
    n_validos = sum(seg_asaltos %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(seg_trafico_drogas %in% c(1:5))),
    n_validos = sum(seg_trafico_drogas %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Invertir escala de variables de seguridad objetiva 
# Original: 1=Nunca, 2=Casi nunca, 3=A veces, 4=Casi siempre, 5=Siempre (problemas)
# Invertido: 1=Siempre, 2=Casi siempre, 3=A veces, 4=Casi nunca, 5=Nunca (problemas)
# Ahora: Valores altos = Mayor seguridad (menos problemas)
db <- db %>%
  mutate(
    seg_peleas_calle = 6 - seg_peleas_calle,
    seg_asaltos = 6 - seg_asaltos,
    seg_trafico_drogas = 6 - seg_trafico_drogas
  )

# Actualizar las etiquetas después de la inversión
etiquetas_invertidas <- c(
  "Siempre" = 1,
  "Casi siempre" = 2, 
  "A veces" = 3,
  "Casi nunca" = 4,
  "Nunca" = 5
)

# Aplicar las nuevas etiquetas a las variables invertidas
db$seg_peleas_calle <- sjlabelled::set_labels(db$seg_peleas_calle, labels = etiquetas_invertidas)
db$seg_asaltos <- sjlabelled::set_labels(db$seg_asaltos, labels = etiquetas_invertidas)
db$seg_trafico_drogas <- sjlabelled::set_labels(db$seg_trafico_drogas, labels = etiquetas_invertidas)

# Verificar la inversión de escala
cat("=== VERIFICACIÓN DE INVERSIÓN DE ESCALA ===\n")
cat("Ahora: 5=Nunca problemas (muy seguro), 1=Siempre problemas (muy inseguro)\n")
cat("Rango de valores después de inversión:\n")
cat("peleas_calle:", range(db$seg_peleas_calle, na.rm = TRUE), "\n")
cat("asaltos:", range(db$seg_asaltos, na.rm = TRUE), "\n")
cat("trafico_drogas:", range(db$seg_trafico_drogas, na.rm = TRUE), "\n")

# Verificar distribución por ola
cat("\nDistribución de peleas_calle por ola (después de inversión):\n")
print(table(db$ola, db$seg_peleas_calle, useNA = "ifany"))

db$seguridad_obj <- rowMeans(db[, c("seg_peleas_calle", "seg_asaltos", "seg_trafico_drogas")], na.rm = TRUE)
db$seguridad_obj <- (round(db$seguridad_obj * 2) / 2)

frq(db$seguridad_obj)

# Seguridad Pública

db %>% 
  group_by(ola) %>% 
  select(seguridad_sub, seguridad_obj) %>% 
  frq()

db$seguridad_pub <- rowMeans(db[, c("seguridad_sub", "seguridad_obj")], na.rm = TRUE)

frq(db$seguridad_pub)

#-----Vínculos Territoriales-----

# sentido_pertenencia

db %>% 
  group_by(ola) %>% 
  select(bar_ideal, bar_integracion, bar_identidad, bar_pertenencia) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_ideal %in% c(1:5))),
    n_validos = sum(bar_ideal %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_integracion %in% c(1:5))),
    n_validos = sum(bar_integracion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_identidad %in% c(1:5))),
    n_validos = sum(bar_identidad %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_pertenencia %in% c(1:5))),
    n_validos = sum(bar_pertenencia %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$sentido_pertenencia <- rowMeans(db[, c("bar_ideal", "bar_integracion", "bar_identidad", "bar_pertenencia")], na.rm = TRUE)
db$sentido_pertenencia <- (round(db$sentido_pertenencia * 2) / 2)

frq(db$sentido_pertenencia)


# satisfaccion_barrio
db %>% 
  group_by(ola) %>% 
  select(bar_amigos, bar_sociable, bar_cordial, bar_colaborador) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_amigos %in% c(1:5))),
    n_validos = sum(bar_amigos %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_sociable %in% c(1:5))),
    n_validos = sum(bar_sociable %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_cordial %in% c(1:5))),
    n_validos = sum(bar_cordial %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(bar_colaborador %in% c(1:5))),
    n_validos = sum(bar_colaborador %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$satisfaccion_barrio <- rowMeans(db[, c("bar_amigos", "bar_sociable", "bar_cordial", "bar_colaborador")], na.rm = TRUE)
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
  select(conf_inst_gobierno, conf_inst_congreso, conf_inst_pp) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_inst_gobierno %in% c(1:5))),
    n_validos = sum(conf_inst_gobierno %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_inst_congreso %in% c(1:5))),
    n_validos = sum(conf_inst_congreso %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(conf_inst_pp %in% c(1:5))),
    n_validos = sum(conf_inst_pp %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$conf_inst_pol <- rowMeans(db[, c("conf_inst_gobierno", "conf_inst_congreso", "conf_inst_pp")], na.rm = TRUE)

frq(db$conf_inst_pol)

#-----Participación Política-----

db %>% 
  group_by(ola) %>% 
  select(part_firma_peticion, part_asiste_marcha, part_huelga, opinion_rrss) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(part_firma_peticion %in% c(1:5))),
    n_validos = sum(part_firma_peticion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(part_asiste_marcha %in% c(1:5))),
    n_validos = sum(part_asiste_marcha %in% c(1:5)),
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

db$pp_politica <- rowMeans(db[, c("part_firma_peticion", "part_asiste_marcha", "part_huelga", "opinion_rrss")], na.rm = TRUE)

frq(db$pp_politica)

#-----Autoeficacia Política-----

db %>% 
  group_by(ola) %>% 
  select(autoef_voto_deber, autoef_voto_influye, autoef_voto_expresion) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(autoef_voto_deber %in% c(1:5))),
    n_validos = sum(autoef_voto_deber %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(autoef_voto_influye %in% c(1:5))),
    n_validos = sum(autoef_voto_influye %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(autoef_voto_expresion %in% c(1:5))),
    n_validos = sum(autoef_voto_expresion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$auto_efic <- rowMeans(db[, c("autoef_voto_deber", "autoef_voto_influye", "autoef_voto_expresion")], na.rm = TRUE)

frq(db$auto_efic)

#-----Interés en Política-----

db %>% 
  group_by(ola) %>% 
  select(intpol_interes_politica, intpol_hablar_politica, intpol_infopolitica_medios) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(intpol_interes_politica %in% c(1:5))),
    n_validos = sum(intpol_interes_politica %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(intpol_hablar_politica %in% c(1:5))),
    n_validos = sum(intpol_hablar_politica %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(intpol_infopolitica_medios %in% c(1:5))),
    n_validos = sum(intpol_infopolitica_medios %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$int_pol <- rowMeans(db[, c("intpol_interes_politica", "intpol_hablar_politica", "intpol_infopolitica_medios")], na.rm = TRUE)

frq(db$int_pol)

#-----Practicas y actitudes politicas-----

db %>% 
  group_by(ola) %>% 
  select(pp_politica, auto_efic, int_pol) %>% 
  frq()

db$prac_acti_pol <- rowMeans(db[, c("pp_politica", "auto_efic", "int_pol")], na.rm = TRUE)

frq(db$prac_acti_pol)

#-----Preferencias Autoritarias-----

db %>% 
  group_by(ola) %>% 
  select(autor_gobierno_firme, autor_mandatario_fuerte, autor_vida_disciplinar) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(autor_gobierno_firme %in% c(1:5))),
    n_validos = sum(autor_gobierno_firme %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(autor_mandatario_fuerte %in% c(1:5))),
    n_validos = sum(autor_mandatario_fuerte %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(autor_vida_disciplinar %in% c(1:5))),
    n_validos = sum(autor_vida_disciplinar %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Invertir escala de variables de preferencias autoritarias
# Original: 1=Totalmente en desacuerdo, 2=En desacuerdo, 3=Ni acuerdo ni desacuerdo, 4=De acuerdo, 5=Totalmente de acuerdo
# Invertido: 1=Totalmente de acuerdo, 2=De acuerdo, 3=Ni acuerdo ni desacuerdo, 4=En desacuerdo, 5=Totalmente en desacuerdo
# Ahora: Valores altos = Menor autoritarismo (más democrático)
db <- db %>%
  mutate(
    autor_gobierno_firme = 6 - autor_gobierno_firme,
    autor_mandatario_fuerte = 6 - autor_mandatario_fuerte,
    autor_vida_disciplinar = 6 - autor_vida_disciplinar
  )

# Actualizar las etiquetas después de la inversión
etiquetas_autoritarias_invertidas <- c(
  "Totalmente de acuerdo" = 1,
  "De acuerdo" = 2,
  "Ni acuerdo ni desacuerdo" = 3,
  "En desacuerdo" = 4,
  "Totalmente en desacuerdo" = 5
)

# Aplicar las nuevas etiquetas a las variables invertidas
db$autor_gobierno_firme <- sjlabelled::set_labels(db$autor_gobierno_firme, labels = etiquetas_autoritarias_invertidas)
db$autor_mandatario_fuerte <- sjlabelled::set_labels(db$autor_mandatario_fuerte, labels = etiquetas_autoritarias_invertidas)
db$autor_vida_disciplinar <- sjlabelled::set_labels(db$autor_vida_disciplinar, labels = etiquetas_autoritarias_invertidas)

# Verificar la inversión
cat("=== VERIFICACIÓN INVERSIÓN PREFERENCIAS AUTORITARIAS ===\n")
cat("Ahora: 5=Totalmente en desacuerdo con autoritarismo (más democrático)\n")
cat("       1=Totalmente de acuerdo con autoritarismo (más autoritario)\n")


db$pref_autor <- rowMeans(db[, c("autor_gobierno_firme", "autor_mandatario_fuerte", "autor_vida_disciplinar")], na.rm = TRUE)

frq(db$pref_autor)

#-----Justicia Distributiva-----

db %>% 
  group_by(ola) %>% 
  select(just_pensiones, just_educacion, just_salud) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(just_pensiones %in% c(1:5))),
    n_validos = sum(just_pensiones %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(just_educacion %in% c(1:5))),
    n_validos = sum(just_educacion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(just_salud %in% c(1:5))),
    n_validos = sum(just_salud %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Invertir escala de variables de justicia distributiva
# Original: 1=Totalmente en desacuerdo, 2=En desacuerdo, 3=Ni acuerdo ni desacuerdo, 4=De acuerdo, 5=Totalmente de acuerdo
# Invertido: 1=Totalmente de acuerdo, 2=De acuerdo, 3=Ni acuerdo ni desacuerdo, 4=En desacuerdo, 5=Totalmente en desacuerdo
# Ahora: Valores altos = Mayor percepción de justicia (más justo)
db <- db %>%
  mutate(
    just_pensiones = 6 - just_pensiones,
    just_educacion = 6 - just_educacion,
    just_salud = 6 - just_salud
  )

# Actualizar las etiquetas después de la inversión
etiquetas_justicia_invertidas <- c(
  "Totalmente de acuerdo" = 1,
  "De acuerdo" = 2,
  "Ni acuerdo ni desacuerdo" = 3,
  "En desacuerdo" = 4,
  "Totalmente en desacuerdo" = 5
)

# Aplicar las nuevas etiquetas a las variables invertidas
db$just_pensiones <- sjlabelled::set_labels(db$just_pensiones, labels = etiquetas_justicia_invertidas)
db$just_educacion <- sjlabelled::set_labels(db$just_educacion, labels = etiquetas_justicia_invertidas)
db$just_salud <- sjlabelled::set_labels(db$just_salud, labels = etiquetas_justicia_invertidas)

# Verificar la inversión
cat("=== VERIFICACIÓN INVERSIÓN JUSTICIA DISTRIBUTIVA ===\n")
cat("Ahora: 5=Totalmente en desacuerdo con injusticia (más justo)\n")
cat("       1=Totalmente de acuerdo con injusticia (menos justo)\n")

db$just_distrib <- rowMeans(db[, c("just_pensiones", "just_educacion", "just_salud")], na.rm = TRUE)

frq(db$just_distrib)

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
variables_recode <- colnames(db[,18:79])

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
print(sjlabelled::get_labels(db$seg_seguridad_sat))

# 3.4 Check BBDD

glimpse(db)

sjPlot::view_df(db,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)

db_madre <- db

save(db_madre, file = here ("data/bases-vis-elsoc/db_madre.RData"))

# 4. Final data -----------------------------------------------------------

#  Database ID and Wave

# ==========================================
# BASE 1a: Promedios por ID de encuesta
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
# BASE 2b: Promedios por ola (sin NAs)
# ==========================================

db_promedios_ola <- db %>%
  select(-c(idencuesta, 
            muestra, 
            tipo_atricion, 
            segmento,
            estrato,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil,
            nhogar1,
            m46_nhogar,
            m54,
            m30,
            m30b,
            m29)) %>%  # Excluir variables
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
            segmento,
            estrato,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil,
            nhogar1,
            m46_nhogar,
            m54,
            m30,
            m30b,
            m29)) %>% 
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
  "just_pensiones", "just_educacion", "just_salud", "just_distrib", 
  "coh_horiz", "coh_vert", "coh_gral"
)

# 2. Recodifica todas las variables de una vez
db_categ <- db %>%
  mutate(across(all_of(todas_las_variables), ~case_when(
    .x >= 3 ~ "Alto",
    .x < 3  ~ "Bajo"
  ))) %>% 
  select(1:19, all_of(todas_las_variables))

db_categ <- db_categ %>% 
  mutate_at(.vars = todas_las_variables, .funs = ~ as_factor(.))

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
            segmento,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil,
            nhogar1,
            m46_nhogar,
            m54,
            m30,
            m30b,
            m29)) %>% 
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
            segmento,
            educacion,
            edad, 
            sexo, 
            ideologia, 
            religion, 
            estado_civil,
            nhogar1,
            m46_nhogar,
            m54,
            m30,
            m30b,
            m29)) %>% 
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

# ==============================================
# BASE 4: Base longitudinal con estructura jerárquica
# ==============================================

# Crear base longitudinal con todos los indicadores
variables_indicadores <- c(
  "seg_peleas_calle", "seg_asaltos", "seg_trafico_drogas",
  "seg_seguridad_sat", "seg_seguridad_perc", 
  "bar_ideal", "bar_integracion", "bar_identidad", "bar_pertenencia",
  "bar_amigos", "bar_sociable", "bar_cordial", "bar_colaborador",
  "conf_inter_general", "conf_inter_altruismo",
  "prosoc_reunion_pub", "prosoc_voluntariado",
  "ayuda_prestar_dinero", "ayuda_trabajo", "ayuda_donar_dinero",
  "conf_inst_gobierno", "conf_inst_pp", "conf_inst_congreso",
  "part_firma_peticion", "part_asiste_marcha", "part_huelga",
  "autoef_voto_deber", "autoef_voto_influye", "autoef_voto_expresion",
  "intpol_interes_politica", "intpol_hablar_politica", "intpol_infopolitica_medios",
  "autor_gobierno_firme", "autor_mandatario_fuerte", "autor_vida_disciplinar",
  "just_pensiones", "just_educacion", "just_salud"
)

# Crear base en formato largo
df_long_jerarquica <- db %>%
  select(ola, all_of(variables_indicadores)) %>%
  pivot_longer(
    cols = -ola,
    names_to = "indicador",
    values_to = "meanvalue"
  ) %>%
  group_by(ola, indicador) %>%
  summarise(meanvalue = mean(meanvalue, na.rm = TRUE), .groups = "drop")

# Crear subdimensiones
df_long_jerarquica$subdimension <- 
car::recode(df_long_jerarquica$indicador, "
  'seg_peleas_calle'       = 'Seguridad objetiva';
  'seg_asaltos'            = 'Seguridad objetiva';
  'seg_trafico_drogas'     = 'Seguridad objetiva';
  
  'seg_seguridad_sat'      = 'Seguridad subjetiva';
  'seg_seguridad_perc'     = 'Seguridad subjetiva';
  
  'bar_ideal'              = 'Pertenencia al Barrio';
  'bar_integracion'        = 'Pertenencia al Barrio';
  'bar_identidad'          = 'Pertenencia al Barrio';
  'bar_pertenencia'        = 'Pertenencia al Barrio';
  
  'bar_amigos'             = 'Satisfacción con el barrio';
  'bar_sociable'           = 'Satisfacción con el barrio';
  'bar_cordial'            = 'Satisfacción con el barrio';
  'bar_colaborador'        = 'Satisfacción con el barrio';
  
  'conf_inter_general'     = 'Confianza interpersonal';
  'conf_inter_altruismo'   = 'Confianza interpersonal';
  
  'prosoc_reunion_pub'     = 'Comportamiento prosocial';
  'prosoc_voluntariado'    = 'Comportamiento prosocial';
  
  'ayuda_prestar_dinero'   = 'Ayuda económica';
  'ayuda_trabajo'          = 'Ayuda económica';
  'ayuda_donar_dinero'     = 'Ayuda económica';
  
  'conf_inst_gobierno'     = 'Confianza en instituciones políticas';
  'conf_inst_pp'           = 'Confianza en instituciones políticas';
  'conf_inst_congreso'     = 'Confianza en instituciones políticas';
  
  'part_firma_peticion'    = 'Participación política';
  'part_asiste_marcha'     = 'Participación política';
  'part_huelga'            = 'Participación política';
  
  'autoef_voto_deber'      = 'Autoeficacia política';
  'autoef_voto_influye'    = 'Autoeficacia política';
  'autoef_voto_expresion'  = 'Autoeficacia política';
  
  'intpol_interes_politica'   = 'Interés en política';
  'intpol_hablar_politica'    = 'Interés en política';
  'intpol_infopolitica_medios'= 'Interés en política';
  
  'autor_gobierno_firme'   = 'Preferencias autoritarias';
  'autor_mandatario_fuerte'= 'Preferencias autoritarias';
  'autor_vida_disciplinar' = 'Preferencias autoritarias';
  
  'just_pensiones'         = 'Justicia distributiva';
  'just_educacion'         = 'Justicia distributiva';
  'just_salud'             = 'Justicia distributiva'
")

# Verificar subdimensiones
sjmisc::frq(df_long_jerarquica$subdimension)

# Crear dimensiones
df_long_jerarquica$dimension <- 
car::recode(df_long_jerarquica$subdimension, "
            'Seguridad objetiva'  = 'Seguridad pública';
            'Seguridad subjetiva' = 'Seguridad pública';
            
            'Satisfacción con el barrio' = 'Vínculos territoriales';
            'Pertenencia al Barrio' = 'Vínculos territoriales';
            
            'Ayuda económica' = 'Redes sociales';
            'Comportamiento prosocial' = 'Redes sociales';
            'Confianza interpersonal' = 'Redes sociales';
            
            'Confianza en instituciones políticas' = 'Confianza en instituciones';            

            'Autoeficacia política' = 'Participación y actitudes políticas';
            'Interés en política' = 'Participación y actitudes políticas';
            'Participación política' = 'Participación y actitudes políticas';   
            
            'Preferencias autoritarias' = 'Preferencia por autoritarismo';
            
            'Justicia distributiva' = 'Justicia distributiva';
            ")

# Verificar dimensiones
sjmisc::frq(df_long_jerarquica$dimension)

# Crear áreas
df_long_jerarquica$area <- 
car::recode(df_long_jerarquica$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

# Verificar áreas
sjmisc::frq(df_long_jerarquica$area)

# Ver resultado final
head(df_long_jerarquica, 20)
glimpse(df_long_jerarquica)

# Guardar base longitudinal
save(df_long_jerarquica, file = here ("data/bases-vis-elsoc/df_long_jerarquica.RData"))

sjPlot::view_df(db_categ,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)


