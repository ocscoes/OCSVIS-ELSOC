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
         in_seg_seguridad_sat = t06_01, 
         in_seg_seguridad_perc = t10, 
         in_seg_peleas_calle = t09_01,
         in_seg_asaltos = t09_02,
         in_seg_trafico_drogas = t09_03,
         in_bar_ideal = t02_01,
         in_bar_integracion = t02_02, 
         in_bar_identidad = t02_03, 
         in_bar_pertenencia = t02_04, 
         in_bar_amigos = t03_01, 
         in_bar_sociable = t03_02, 
         in_bar_cordial = t03_03, 
         in_bar_colaborador = t03_04, 
         in_conf_inter_general = c02,
         in_conf_inter_altruismo = c03,
         in_prosoc_reunion_pub = c07_02, 
         in_prosoc_voluntariado = c07_04,
         in_ayuda_donar_dinero = c07_05,
         in_ayuda_prestar_dinero = c07_06, 
         in_ayuda_trabajo = c07_08,
         in_conf_inst_gobierno = c05_01, 
         in_conf_inst_pp = c05_02, 
         in_conf_judicial = c05_05, 
         in_conf_inst_congreso = c05_07, 
         in_part_firma_peticion = c08_01, 
         in_part_asiste_marcha = c08_02, 
         in_part_huelga = c08_03,
         in_opinion_rrss = c08_04, 
         in_autoef_voto_deber = c10_01, 
         in_autoef_voto_influye = c10_02, 
         in_autoef_voto_expresion = c10_03, 
         in_intpol_interes_politica = c13, 
         in_intpol_hablar_politica = c14_01, 
         in_intpol_infopolitica_medios = c14_02, 
         in_autor_gobierno_firme = c18_04, 
         in_autor_mandatario_fuerte = c18_05, 
         in_autor_vida_disciplinar = c18_07, 
         in_just_pensiones = d02_01,
         in_just_educacion = d02_02, 
         in_just_salud = d02_03, 
         in_sat_democracia = c01) %>% 
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
  select(in_prosoc_reunion_pub, in_prosoc_voluntariado) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_prosoc_reunion_pub %in% c(1,2,3))),
    n_validos = sum(in_prosoc_reunion_pub %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_prosoc_voluntariado %in% c(1,2,3))),
    n_validos = sum(in_prosoc_voluntariado %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes (una por una )
variables_a_transformar <- c("in_prosoc_reunion_pub", "in_prosoc_voluntariado")

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

db$sd_comportamiento_prosocial <- rowMeans(db[, c("in_prosoc_reunion_pub", "in_prosoc_voluntariado")], na.rm = TRUE)

frq(db$sd_comportamiento_prosocial)

# ayuda_economica

db %>% 
  group_by(ola) %>% 
  select(in_ayuda_prestar_dinero, in_ayuda_trabajo, in_ayuda_donar_dinero) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_ayuda_prestar_dinero %in% c(1,2,3))),
    n_validos = sum(in_ayuda_prestar_dinero %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_ayuda_trabajo %in% c(1,2,3))),
    n_validos = sum(in_ayuda_trabajo %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_ayuda_donar_dinero %in% c(1,2,3))),
    n_validos = sum(in_ayuda_donar_dinero %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes
variables_a_transformar <- c("in_ayuda_prestar_dinero", "in_ayuda_trabajo", "in_ayuda_donar_dinero")

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

db$sd_ayuda_economica <- rowMeans(db[, c("in_ayuda_prestar_dinero", "in_ayuda_trabajo", "in_ayuda_donar_dinero")], na.rm = TRUE)

frq(db$sd_ayuda_economica)

# confianza_inter

db %>% 
  group_by(ola) %>% 
  select(in_conf_inter_general, in_conf_inter_altruismo) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_conf_inter_general %in% c(1,2,3))),
    n_validos = sum(in_conf_inter_general %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_conf_inter_altruismo %in% c(1,2,3))),
    n_validos = sum(in_conf_inter_altruismo %in% c(1,2,3)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Recode Redes - Primero recodificar orden correcto (1=1, 2=3, 3=2)
variables_a_transformar <- c("in_conf_inter_general", "in_conf_inter_altruismo")

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

db$sd_confianza_inter <- rowMeans(db[, c("in_conf_inter_general", "in_conf_inter_altruismo")], na.rm = TRUE)

frq(db$sd_confianza_inter)


# Redes Sociales
db %>% 
  group_by(ola) %>% 
  select(sd_comportamiento_prosocial, sd_ayuda_economica, sd_confianza_inter) %>% 
  frq()

db$di_redes_sociales <- rowMeans(db[, c("sd_comportamiento_prosocial", "sd_ayuda_economica", "sd_confianza_inter")], na.rm = TRUE)

frq(db$di_redes_sociales)

#----Seguridad-----

# seguridad_sub

db %>% 
  group_by(ola) %>% 
  select(in_seg_seguridad_sat, in_seg_seguridad_perc) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_seg_seguridad_sat %in% c(1:5))),
    n_validos = sum(in_seg_seguridad_sat %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_seg_seguridad_perc %in% c(1:5))),
    n_validos = sum(in_seg_seguridad_perc %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$sd_seguridad_sub <- rowMeans(db[, c("in_seg_seguridad_sat", "in_seg_seguridad_perc")], na.rm = TRUE)

frq(db$sd_seguridad_sub)

# seguridad_obj

db %>% 
  group_by(ola) %>% 
  select(in_seg_peleas_calle, in_seg_asaltos, in_seg_trafico_drogas) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_seg_peleas_calle %in% c(1:5))),
    n_validos = sum(in_seg_peleas_calle %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_seg_asaltos %in% c(1:5))),
    n_validos = sum(in_seg_asaltos %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_seg_trafico_drogas %in% c(1:5))),
    n_validos = sum(in_seg_trafico_drogas %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Invertir escala de variables de seguridad objetiva 
# Original: 1=Nunca, 2=Casi nunca, 3=A veces, 4=Casi siempre, 5=Siempre (problemas)
# Invertido: 1=Siempre, 2=Casi siempre, 3=A veces, 4=Casi nunca, 5=Nunca (problemas)
# Ahora: Valores altos = Mayor seguridad (menos problemas)

db <- db %>%
  mutate(
    in_seg_peleas_calle = sjmisc::rec(in_seg_peleas_calle, rec = "rev"),
    in_seg_asaltos = sjmisc::rec(in_seg_asaltos, rec = "rev"),
    in_seg_trafico_drogas = sjmisc::rec(in_seg_trafico_drogas, rec = "rev"))

frq(db$in_seg_peleas_calle)
frq(db$in_seg_asaltos)
frq(db$in_seg_trafico_drogas)

db$sd_seguridad_obj <- rowMeans(db[, c("in_seg_peleas_calle", "in_seg_asaltos", "in_seg_trafico_drogas")], na.rm = TRUE)
db$sd_seguridad_obj <- (round(db$sd_seguridad_obj * 2) / 2)

frq(db$sd_seguridad_obj)

# Seguridad Pública

db %>% 
  group_by(ola) %>% 
  select(sd_seguridad_sub, sd_seguridad_obj) %>% 
  frq()

db$di_seguridad_pub <- rowMeans(db[, c("sd_seguridad_sub", "sd_seguridad_obj")], na.rm = TRUE)

frq(db$di_seguridad_pub)

#-----Vínculos Territoriales-----

# sentido_pertenencia

db %>% 
  group_by(ola) %>% 
  select(in_bar_ideal, in_bar_integracion, in_bar_identidad, in_bar_pertenencia) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_ideal %in% c(1:5))),
    n_validos = sum(in_bar_ideal %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_integracion %in% c(1:5))),
    n_validos = sum(in_bar_integracion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_identidad %in% c(1:5))),
    n_validos = sum(in_bar_identidad %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_pertenencia %in% c(1:5))),
    n_validos = sum(in_bar_pertenencia %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$sd_sentido_pertenencia <- rowMeans(db[, c("in_bar_ideal", "in_bar_integracion", "in_bar_identidad", "in_bar_pertenencia")], na.rm = TRUE)
db$sd_sentido_pertenencia <- (round(db$sd_sentido_pertenencia * 2) / 2)

frq(db$sd_sentido_pertenencia)


# satisfaccion_barrio
db %>% 
  group_by(ola) %>% 
  select(in_bar_amigos, in_bar_sociable, in_bar_cordial, in_bar_colaborador) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_amigos %in% c(1:5))),
    n_validos = sum(in_bar_amigos %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_sociable %in% c(1:5))),
    n_validos = sum(in_bar_sociable %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_cordial %in% c(1:5))),
    n_validos = sum(in_bar_cordial %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_bar_colaborador %in% c(1:5))),
    n_validos = sum(in_bar_colaborador %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$sd_satisfaccion_barrio <- rowMeans(db[, c("in_bar_amigos", "in_bar_sociable", "in_bar_cordial", "in_bar_colaborador")], na.rm = TRUE)
db$sd_satisfaccion_barrio <- (round(db$sd_satisfaccion_barrio * 2) / 2)

frq(db$sd_satisfaccion_barrio)

# Vínculos Territoriales

db %>% 
  group_by(ola) %>% 
  select(sd_sentido_pertenencia, sd_satisfaccion_barrio) %>% 
  frq()

db$di_vinculos_territ <- rowMeans(db[, c("sd_sentido_pertenencia", "sd_satisfaccion_barrio")], na.rm = TRUE)
frq(db$di_vinculos_territ)

# COHESIÓN VERTICAL

#-----Confianza en Instituciones Políticas-----

db %>% 
  group_by(ola) %>% 
  select(in_conf_inst_gobierno, in_conf_inst_congreso, in_conf_inst_pp) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_conf_inst_gobierno %in% c(1:5))),
    n_validos = sum(in_conf_inst_gobierno %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_conf_inst_congreso %in% c(1:5))),
    n_validos = sum(in_conf_inst_congreso %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_conf_inst_pp %in% c(1:5))),
    n_validos = sum(in_conf_inst_pp %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$sd_conf_inst_pol <- rowMeans(db[, c("in_conf_inst_gobierno", "in_conf_inst_congreso", "in_conf_inst_pp")], na.rm = TRUE)

frq(db$sd_conf_inst_pol)
db$di_conf_inst_pol <- db$sd_conf_inst_pol

#-----Participación Política-----

db %>% 
  group_by(ola) %>% 
  select(in_part_firma_peticion, in_part_asiste_marcha, in_part_huelga, in_opinion_rrss) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_part_firma_peticion %in% c(1:5))),
    n_validos = sum(in_part_firma_peticion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_part_asiste_marcha %in% c(1:5))),
    n_validos = sum(in_part_asiste_marcha %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_part_huelga %in% c(1:5))),
    n_validos = sum(in_part_huelga %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_opinion_rrss %in% c(1:5))),
    n_validos = sum(in_opinion_rrss %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$sd_pp_politica <- rowMeans(db[, c("in_part_firma_peticion", "in_part_asiste_marcha", "in_part_huelga", "in_opinion_rrss")], na.rm = TRUE)

frq(db$sd_pp_politica)

#-----Autoeficacia Política-----

db %>% 
  group_by(ola) %>% 
  select(in_autoef_voto_deber, in_autoef_voto_influye, in_autoef_voto_expresion) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_autoef_voto_deber %in% c(1:5))),
    n_validos = sum(in_autoef_voto_deber %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_autoef_voto_influye %in% c(1:5))),
    n_validos = sum(in_autoef_voto_influye %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_autoef_voto_expresion %in% c(1:5))),
    n_validos = sum(in_autoef_voto_expresion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db$sd_auto_efic <- rowMeans(db[, c("in_autoef_voto_deber", "in_autoef_voto_influye", "in_autoef_voto_expresion")], na.rm = TRUE)

frq(db$sd_auto_efic)

#-----Interés en Política-----

db %>% 
  group_by(ola) %>% 
  select(in_intpol_interes_politica, in_intpol_hablar_politica, in_intpol_infopolitica_medios) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_intpol_interes_politica %in% c(1:5))),
    n_validos = sum(in_intpol_interes_politica %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_intpol_hablar_politica %in% c(1:5))),
    n_validos = sum(in_intpol_hablar_politica %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_intpol_infopolitica_medios %in% c(1:5))),
    n_validos = sum(in_intpol_infopolitica_medios %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )


db$sd_int_pol <- rowMeans(db[, c("in_intpol_interes_politica", "in_intpol_hablar_politica", "in_intpol_infopolitica_medios")], na.rm = TRUE)

frq(db$sd_int_pol)

#-----Practicas y actitudes politicas-----

db %>% 
  group_by(ola) %>% 
  select(sd_pp_politica, sd_auto_efic, sd_int_pol) %>% 
  frq()

db$di_prac_acti_pol <- rowMeans(db[, c("sd_pp_politica", "sd_auto_efic", "sd_int_pol")], na.rm = TRUE)

frq(db$di_prac_acti_pol)

#-----Preferencias Autoritarias-----

db %>% 
  group_by(ola) %>% 
  select(in_autor_gobierno_firme, in_autor_mandatario_fuerte, in_autor_vida_disciplinar) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_autor_gobierno_firme %in% c(1:5))),
    n_validos = sum(in_autor_gobierno_firme %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_autor_mandatario_fuerte %in% c(1:5))),
    n_validos = sum(in_autor_mandatario_fuerte %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_autor_vida_disciplinar %in% c(1:5))),
    n_validos = sum(in_autor_vida_disciplinar %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Invertir escala de variables de preferencias autoritarias
# Original: 1=Totalmente en desacuerdo, 2=En desacuerdo, 3=Ni acuerdo ni desacuerdo, 4=De acuerdo, 5=Totalmente de acuerdo
# Invertido: 1=Totalmente de acuerdo, 2=De acuerdo, 3=Ni acuerdo ni desacuerdo, 4=En desacuerdo, 5=Totalmente en desacuerdo
# Ahora: Valores altos = Menor autoritarismo (más democrático)

db %>% 
  select(in_autor_gobierno_firme, in_autor_mandatario_fuerte, in_autor_vida_disciplinar) %>% 
  frq()

db <- db %>%
  mutate(
    in_autor_gobierno_firme = sjmisc::rec(in_autor_gobierno_firme, rec = "rev"),
    in_autor_mandatario_fuerte = sjmisc::rec(in_autor_mandatario_fuerte, rec = "rev"),
    in_autor_vida_disciplinar = sjmisc::rec(in_autor_vida_disciplinar, rec = "rev"))

db %>% 
  select(in_autor_gobierno_firme, in_autor_mandatario_fuerte, in_autor_vida_disciplinar) %>% 
  frq()

db$sd_pref_autor <- rowMeans(db[, c("in_autor_gobierno_firme", "in_autor_mandatario_fuerte", "in_autor_vida_disciplinar")], na.rm = TRUE)

frq(db$sd_pref_autor)
db$di_pref_autor <- db$sd_pref_autor

#-----Justicia Distributiva-----

db %>% 
  group_by(ola) %>% 
  select(in_just_pensiones, in_just_educacion, in_just_salud) %>% 
  frq()

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_just_pensiones %in% c(1:5))),
    n_validos = sum(in_just_pensiones %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_just_educacion %in% c(1:5))),
    n_validos = sum(in_just_educacion %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

db %>%
  group_by(ola) %>%
  summarise(
    n_total = n(),
    n_invalidos = sum(!(in_just_salud %in% c(1:5))),
    n_validos = sum(in_just_salud %in% c(1:5)),
    total = if_else(sum(n_invalidos, n_validos) == n_total, TRUE, FALSE),
    .groups = "drop"
  )

# Invertir escala de variables de justicia distributiva
# Original: 1=Totalmente en desacuerdo, 2=En desacuerdo, 3=Ni acuerdo ni desacuerdo, 4=De acuerdo, 5=Totalmente de acuerdo
# Invertido: 1=Totalmente de acuerdo, 2=De acuerdo, 3=Ni acuerdo ni desacuerdo, 4=En desacuerdo, 5=Totalmente en desacuerdo
# Ahora: Valores altos = Mayor percepción de justicia (más justo)

db <- db %>%
  mutate(
    in_just_pensiones = sjmisc::rec(in_just_pensiones, rec = "rev"),
    in_just_educacion = sjmisc::rec(in_just_educacion, rec = "rev"),
    in_just_salud = sjmisc::rec(in_just_salud, rec = "rev"))

db$sd_just_distrib <- rowMeans(db[, c("in_just_pensiones", "in_just_educacion", "in_just_salud")], na.rm = TRUE)

frq(db$sd_just_distrib)

db$di_just_distrib <- db$sd_just_distrib

###COHESIÓN HORIZONTAL###

db %>% 
  group_by(ola) %>% 
  select(di_seguridad_pub, di_vinculos_territ, di_redes_sociales) %>% 
  frq()

db$ar_coh_horiz <- rowMeans(db[, c("di_seguridad_pub", "di_vinculos_territ", "di_redes_sociales")], na.rm = TRUE)
hist(db$ar_coh_horiz)

psych::describeBy(db$ar_coh_horiz, group = db$ola)

###COHESION VERTICAL###
db %>% 
  group_by(ola) %>% 
  select(sd_conf_inst_pol, sd_pp_politica, sd_auto_efic, sd_int_pol, sd_pref_autor, sd_just_distrib) %>% 
  frq()

db$ar_coh_vert <- rowMeans(db[, c("sd_conf_inst_pol", "sd_pp_politica", "sd_auto_efic", "sd_int_pol", "sd_pref_autor", "sd_just_distrib")], na.rm = TRUE)
hist(db$ar_coh_vert)

psych::describeBy(db$ar_coh_vert, group = db$ola)

###COHESION GENERAL###

db %>% 
  group_by(ola) %>% 
  select(ar_coh_horiz, ar_coh_vert) %>% 
  frq()

db$ar_coh_gral <- rowMeans(db[, c("ar_coh_horiz", "ar_coh_vert")], na.rm = TRUE)

hist(db$ar_coh_gral)
psych::describeBy(db$ar_coh_gral, group = db$ola)

# Label Variables

names(db)

# 1. Vector
variables_recode <- colnames(db[,19:82])

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
frq(db$sd_seguridad_sub)
frq(db$in_seg_seguridad_sat)


################
# COVARIABLES -------------------------------------------------------------
################

# 3.1 Educacion

frq(db$educacion)

db <- db %>% 
  mutate(cine = case_when(educacion %in% c(1,2,3) ~ "Primaria o menos",
                          educacion %in% c(4,5) ~ "Secundaria",
                          educacion %in% c(6,7) ~ "Técnica",
                          educacion %in% c(8,9,10) ~ "Universitaria o más",
                          TRUE ~ NA_character_),
         cine = factor(cine, 
                       levels = c("Primaria o menos",
                                  "Secundaria",
                                  "Técnica",
                                  "Universitaria o más")))

db$cine <-
  sjlabelled::set_label(x = db$cine,
                        label = "Educación (CINE)")

frq(db$cine)

db$educ_dic <-
  car::recode(db$educacion,
              "c(1,2,3,4,5,6,7)=1;c(8,9,10)=2; c(-888,-999)=NA")

db$educ_dic <-
  factor(db$educ_dic,
         labels = c("Menor que universitaria","Universitaria"))

db$educ_dic <-
  sjlabelled::set_label(x = db$educ_dic,
                        label = "Educación (dicotómica)")

frq(db$educ_dic)

#Recoding of education to years based on casen 2017.

db$educyear<- as.numeric(
  car::recode(db$educacion,
              "1=0;2=4.3;3=7.5;4=9.8;5=12.02;6=13.9;
               7=14.8;8=14.9;9=16.9;10=19.07;c(-888,-999)=NA",
              as.numeric = T))
db$educyear <-
  sjlabelled::set_label(x = db$educyear,
                        label = "Educación en años")

frq(db$educyear)


# 3.2 Sexo
frq(db$sexo)

db$sexo <- car::recode(db$sexo, 
                       recodes = c("1='Hombre'; 2='Mujer'"), 
                       levels = c("Hombre", "Mujer"),
                       as.factor = T)

db$sexo <- sjlabelled::set_label(db$sexo, 
                                 label = "Sexo")
frq(db$sexo)

# 3.3 Edad
frq(db$edad)

db$edad_t <- 
  factor(car::recode(db$edad, 
                     "18:29=1;30:49=2;50:64=3;65:150=4"),
         labels = c('18-29', '30-49', '50-64', '65 o más'))

db$edad_t <-
  sjlabelled::set_label(db$edad_t, 
                        label = c("Edad (tramos)")) 

frq(db$edad_t)

# 3.4 Ideologia
frq(db$ideologia)

db$ideologia<-
  factor(
    car::recode(
      db$ideologia,
      "c(11,12,-888,-999)='No se identifica';c(0,1,2,3,4)='Izquierda';
     c(5)='Centro';c(6,7,8,9,10)='Derecha'"
    ),
    levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica')
  )

db$ideologia<- factor(db$ideologia,levels = levels(db$ideologia))

db$ideologia <- 
  sjlabelled::set_label(x = db$ideologia, 
                        label = "Identificación política") 

frq(db$ideologia)

# 3.5 Religion
frq(db$religion)

db <- db %>% 
  mutate(religion = case_when(religion == 1 ~ "Catolico",
                              religion == 2 ~ "Evangelico",
                              religion == 3 ~ "Protestante",
                              religion == 4 ~ "Judio",
                              religion == 5 ~ "Creyente no adherente",
                              religion == 6 ~ "Otra",
                              religion == 7 ~ "Ateo",
                              religion == 8 ~ "Agnostico",
                              religion == 9 ~ "Ninguna",
                              TRUE ~ NA_character_
                              
  )) 

db$religion <- 
  sjlabelled::set_label(x = db$religion, 
                        label = "Identificación religiosa") 

frq(db$religion)

# 3.6 Estado civil

frq(db$estado_civil)

db <- db %>% 
  mutate(estado_civil = case_when(
    estado_civil %in% c(1,2,3) ~ "Casado/Conviviente",
    estado_civil == 4 ~ "Soltero",
    estado_civil %in% c(5,6,7,8,9) ~ "Separado/Divorciado/Viudo/Anulado/Otro",
    TRUE ~ NA_character_
    
  )) 

db$estado_civil <- 
  sjlabelled::set_label(x = db$estado_civil, 
                        label = "Estado civil") 

frq(db$estado_civil)

# 3.7 Ingresos

# N Household:
# Select variables______________________________________________________________
# Household income_________________________________________

#Impute midpoint of income ranges
db$m30_rec <-
  as.numeric(car::recode(db$m30,
                         "1=110000;2=251000;3=305000;4=355000;5=400000;
            6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
            13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
            19=2275000;20=2700000;NA=NA;c(-888,-999)=NA"))

#Impute midpoint of income ranges (2021)
db$m30b_rec <-
  as.numeric(car::recode(db$m30b,
                         "1=125000;2=300000;3=400000;4=575000;5=700000;NA=NA;c(-888,-999)=NA"))

sjmisc::frq(db$m30_rec)
sjmisc::frq(db$m30b_rec)

#Recode DK/DA of Income to NA
db$m29_rec <-
  as.numeric(car::recode(db$m29,"c(-888,-999)=NA"))

#replace NA of income with new imputed variable
db$m29_imp <- 
  ifelse(test = !is.na(db$m29_rec),
         yes =  db$m29_rec,
         no =  db$m30_rec)
summary(db$m29_imp)

db$m29_imp <- 
  ifelse(test = is.na(db$m29_imp),
         yes =  db$m30b_rec,
         no =  db$m29_imp)
summary(db$m29_imp)

# deflate at each year's prices
library(rvest)

url <- "https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/IPC_EMP_2023/638415285164039007?cbFechaInicio=2016&cbFechaTermino=2025&cbFrecuencia=MONTHLY&cbCalculo=NONE&cbFechaBase="

ipc <- url %>%
  read_html() %>%
  html_node("table") %>%
  html_table() %>% 
  rename_with(., ~ tolower(gsub(".", "_", .x, fixed = TRUE))) %>% 
  filter(serie == "Índice IPC General") %>% 
  mutate(
    across(
      .cols = c(everything(), -serie),
      .fns = ~ as.numeric(str_replace(., ",", "."))
    )) %>% 
  select(-sel_) %>% 
  pivot_longer(., cols = -serie,
               names_to = "ano_mes",
               values_to = "ipc") %>% 
  tidyr::separate(col = "ano_mes", into = c("mes", "ano"))

ipc <- ipc %>% 
  filter(mes == "dic") %>% 
  select(ano, ipc)

db <- left_join(db, ipc, by = c("ola" = "ano"))

frq(db$ipc)

# Reshape long to wide
db_wide <- db %>% 
  tidyr::pivot_wider(id_cols = c("idencuesta","muestra"),
                     names_from = "ola",
                     values_from = names(select(db, tipo_atricion:ipc))
  )

db_wide$m54_2022 <- db_wide$m54_2023

# reshape from long to wide
db_long <- db_wide %>%
  pivot_longer(
    cols = -c(idencuesta, muestra),
    names_to = c(".value", "ola"),
    # Toma TODO lo que va antes del último "_" como nombre de variable,
    # y lo que va después como la ola (1..7)
    names_pattern = "^(.*)_(\\d+)$",
    values_drop_na = T
  ) %>%
  mutate(ola = as.integer(ola))

db_long <-
  db_long %>%
  mutate(n_hogar =
           dplyr::case_when(ola == 2016 ~ nhogar1,
                            ola == 2017 ~ m46_nhogar,
                            ola == 2018 ~ m54,
                            ola == 2019 ~ m54,
                            ola == 2021 ~ m54,
                            ola == 2022 ~ m54,
                            ola == 2023 ~ m54))
sjmisc::frq(db_long$n_hogar)

#Recode DK/DA to NA
db_long$n_hogar_r<-
  car::recode(db_long$n_hogar,"c(-888,-999)=NA")

# Per capita household income:
db_long$ing_pc <- 
  (db_long$m29_imp/db_long$n_hogar_r)

db_long$ing_pc <-
  sjlabelled::set_label(x = db_long$ing_pc,
                        label = "Ingreso por hogar per cápita")  

sjmisc::descr(db_long$ing_pc)

# Compute income groups: 10% bottom, 40% lower middle, 40% upper middle, 10% top
db_long <- db_long %>% 
  group_by(ola) %>% 
  mutate(
    percentil = ntile(ing_pc, 10),
    grupo_ingreso = case_when(
      percentil %in% c(1, 2, 3, 4) ~ "40% de menores ingresos",
      percentil %in% c(5, 6, 7, 8, 9) ~ "50% de ingresos intermedios", 
      percentil == 10 ~ "10% de mayores ingresos",
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup()

db_long$grupo_ingreso <- 
  factor(db_long$grupo_ingreso,
         levels = c("40% de menores ingresos",
                    "50% de ingresos intermedios",
                    "10% de mayores ingresos"))

db_long$grupo_ingreso <- 
  sjlabelled::set_label(x = db_long$grupo_ingreso,
                        label = "Grupos de ingreso")  

sjmisc::frq(db_long$grupo_ingreso)

# Include missing cases for income groups
db_long$grupo_ingreso1 <- 
  car::recode(db_long$grupo_ingreso, 
              "'40% de menores ingresos'='40% de menores ingresos';
              '50% de ingresos intermedios'='50% de ingresos intermedios';
              '10% de mayores ingresos'='10% de mayores ingresos';
              'Top 10%'='Top 10%';
              NA='GNA'")

db_long$grupo_ingreso1 <- 
  sjlabelled::set_label(x = db_long$grupo_ingreso1,
                        label = "Grupos de ingreso con NA")  

sjmisc::frq(db_long$grupo_ingreso1)

frq(db_long$ola) #ok

# 3.4 Check BBDD

glimpse(db_long)

sjPlot::view_df(db_long,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)

db_long <- db_long %>%
  group_by(idencuesta) %>%             # Agrupar por el identificador del participante
  mutate(n_participaciones = n()) %>%  # Contar el número de filas (participaciones) por participante
  ungroup()

db_long <- db_long %>% 
  filter(n_participaciones>=3) %>% 
  select(-n_participaciones) # quedarse con casos que hayan participado >=3 veces

# 4. Final data -----------------------------------------------------------

#  Database ID and Wave

# ==========================================
# BASE 1a: Promedios por ID de encuesta
# ==========================================

db_long <- db_long %>% 
  select(-c(13:18,87:94))

glimpse(db_long)

db_madre <- db_long

save(db_madre, file = here ("data/bases-vis-elsoc/db_madre.RData"))

# ==========================================
# BASE 2b: Promedios por ola (sin NAs)
# ==========================================

db_promedios_ola <- db_long %>%
  group_by(ola) %>%
  summarise(
    across(
      .cols = starts_with(c("in_", "sd_", "di_", "ar_")),
      .fns = ~mean(., na.rm = TRUE),
                   .names = "{.col}"))

# Ver resultado
head(db_promedios_ola)
dim(db_promedios_ola)

# ==========================================
# Guardar las bases resultantes
# ==========================================

# En RData
save(db_promedios_ola, file = here ("data/bases-vis-elsoc/db_promedios_ola.RData"))

# ==============================================
# BASES: Recodificaciones Alto-Medio-Bajo (1-5)
# ==============================================

# 1. Define el vector con todas las variables que vas a modificar
todas_las_variables <- c(
  "sd_seguridad_sub", "sd_seguridad_obj", "di_seguridad_pub",
  "sd_sentido_pertenencia", "sd_satisfaccion_barrio", "di_vinculos_territ",
  "sd_comportamiento_prosocial", "sd_ayuda_economica", "sd_confianza_inter", "di_redes_sociales",
  "sd_conf_inst_pol", "di_conf_inst_pol",
  "sd_pp_politica", "sd_auto_efic", "sd_int_pol", "di_prac_acti_pol",
  "sd_pref_autor", "di_pref_autor",
  "sd_just_distrib", "di_just_distrib",
  "ar_coh_horiz", "ar_coh_vert", "ar_coh_gral"
)

# 2. Recodifica todas las variables de una vez
db_categ <- db_long %>%
  mutate(across(all_of(todas_las_variables), ~case_when(
    .x >= 3 ~ "Alto",
    .x < 3  ~ "Bajo"
  ))) %>% 
  select(c(1:12, 76:83),all_of(todas_las_variables))

db_categ <- db_categ %>% 
  mutate_at(.vars = todas_las_variables, .funs = ~ as_factor(.))

# 3. (Opcional) Revisa el resultado en una de las variables
frq(db_categ$sd_seguridad_sub)

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
  select(ola, all_of(todas_las_variables)) %>% 
  pivot_longer(
    cols = -ola,
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

# ==============================================
# BASE 4: Base longitudinal con estructura jerárquica promedios
# ==============================================

# Crear base longitudinal con todos los indicadores
variables_indicadores <- c(
  "in_seg_peleas_calle", "in_seg_asaltos", "in_seg_trafico_drogas",
  "in_seg_seguridad_sat", "in_seg_seguridad_perc", 
  "in_bar_ideal", "in_bar_integracion", "in_bar_identidad", "in_bar_pertenencia",
  "in_bar_amigos", "in_bar_sociable", "in_bar_cordial", "in_bar_colaborador",
  "in_conf_inter_general", "in_conf_inter_altruismo",
  "in_prosoc_reunion_pub", "in_prosoc_voluntariado",
  "in_ayuda_prestar_dinero", "in_ayuda_trabajo", "in_ayuda_donar_dinero",
  "in_conf_inst_gobierno", "in_conf_inst_pp", "in_conf_inst_congreso",
  "in_part_firma_peticion", "in_part_asiste_marcha", "in_part_huelga",
  "in_autoef_voto_deber", "in_autoef_voto_influye", "in_autoef_voto_expresion",
  "in_intpol_interes_politica", "in_intpol_hablar_politica", "in_intpol_infopolitica_medios",
  "in_autor_gobierno_firme", "in_autor_mandatario_fuerte", "in_autor_vida_disciplinar",
  "in_just_pensiones", "in_just_educacion", "in_just_salud"
)

# Crear base en formato largo
df_long_jerarquica <- db_long %>%
  select(idencuesta, ola, all_of(variables_indicadores)) %>%
  pivot_longer(
    cols = -c(idencuesta, ola),
    names_to = "indicador",
    values_to = "meanvalue"
  ) %>%
  group_by(ola, indicador) %>%
  summarise(meanvalue = mean(meanvalue, na.rm = TRUE), .groups = "drop")

# Crear subdimensiones
df_long_jerarquica$subdimension <- 
car::recode(df_long_jerarquica$indicador, "
  'in_seg_peleas_calle'       = 'Seguridad objetiva';
  'in_seg_asaltos'            = 'Seguridad objetiva';
  'in_seg_trafico_drogas'     = 'Seguridad objetiva';
  
  'in_seg_seguridad_sat'      = 'Seguridad subjetiva';
  'in_seg_seguridad_perc'     = 'Seguridad subjetiva';
  
  'in_bar_ideal'              = 'Pertenencia al Barrio';
  'in_bar_integracion'        = 'Pertenencia al Barrio';
  'in_bar_identidad'          = 'Pertenencia al Barrio';
  'in_bar_pertenencia'        = 'Pertenencia al Barrio';
  
  'in_bar_amigos'             = 'Satisfacción con el barrio';
  'in_bar_sociable'           = 'Satisfacción con el barrio';
  'in_bar_cordial'            = 'Satisfacción con el barrio';
  'in_bar_colaborador'        = 'Satisfacción con el barrio';
  
  'in_conf_inter_general'     = 'Confianza interpersonal';
  'in_conf_inter_altruismo'   = 'Confianza interpersonal';
  
  'in_prosoc_reunion_pub'     = 'Comportamiento prosocial';
  'in_prosoc_voluntariado'    = 'Comportamiento prosocial';
  
  'in_ayuda_prestar_dinero'   = 'Ayuda económica';
  'in_ayuda_trabajo'          = 'Ayuda económica';
  'in_ayuda_donar_dinero'     = 'Ayuda económica';
  
  'in_conf_inst_gobierno'     = 'Confianza en instituciones políticas';
  'in_conf_inst_pp'           = 'Confianza en instituciones políticas';
  'in_conf_inst_congreso'     = 'Confianza en instituciones políticas';
  
  'in_part_firma_peticion'    = 'Participación política';
  'in_part_asiste_marcha'     = 'Participación política';
  'in_part_huelga'            = 'Participación política';
  
  'in_autoef_voto_deber'      = 'Autoeficacia política';
  'in_autoef_voto_influye'    = 'Autoeficacia política';
  'in_autoef_voto_expresion'  = 'Autoeficacia política';

  'in_intpol_interes_politica'   = 'Interés en política';
  'in_intpol_hablar_politica'    = 'Interés en política';
  'in_intpol_infopolitica_medios'= 'Interés en política';
  
  'in_autor_gobierno_firme'   = 'Preferencias autoritarias';
  'in_autor_mandatario_fuerte'= 'Preferencias autoritarias';
  'in_autor_vida_disciplinar' = 'Preferencias autoritarias';
  
  'in_just_pensiones'         = 'Justicia distributiva';
  'in_just_educacion'         = 'Justicia distributiva';
  'in_just_salud'             = 'Justicia distributiva'
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

# ==============================================
# BASE 4: Base longitudinal con estructura jerárquica categoricas
# ==============================================

df_categ_jerarquica <- db_categ %>% 
  select(ola, 21:42) %>% 
  pivot_longer(
    cols = -ola,
    names_to = "variable",
    values_to = "valor"
  ) %>% 
  na.omit()

df_categ_jerarquica <- df_categ_jerarquica %>%
  group_by(ola, variable, valor) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

df_categ_jerarquica <- df_categ_jerarquica %>%
  pivot_wider(
    names_from = variable,
    values_from = c(n, prop),
    values_fill = NA,
    names_glue = "{variable}_{.value}"
  )

# Crear base longitudinal con todos los indicadores
variables_indicadores <- colnames(df_categ_jerarquica)[c(3:46)]

df_categ_jerarquica <- df_categ_jerarquica %>%
    pivot_longer(
      cols = matches("^(ar|di|sd)_"),                   
      names_to = c("prefijo", "indicador", ".value"),   
      names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
    )

df_categ_jerarquica <- df_categ_jerarquica %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

df_categ_jerarquica <- df_categ_jerarquica %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
df_categ_jerarquica$subdimension <- 
  car::recode(df_categ_jerarquica$indicador, "
  'sd_seguridad_obj'      = 'Seguridad objetiva';
  'sd_seguridad_sub'      = 'Seguridad subjetiva';

  'sd_sentido_pertenencia'      = 'Pertenencia al Barrio';
  'sd_satisfaccion_barrio'      = 'Satisfacción con el barrio';

  'sd_confianza_inter'     = 'Confianza interpersonal';
  'sd_comportamiento_prosocial'  = 'Comportamiento prosocial';
  'sd_ayuda_economica'   = 'Ayuda económica';

  'sd_conf_inst_pol'     = 'Confianza en instituciones políticas';
  
  'sd_pp_politica'    = 'Participación política';
  'sd_auto_efic'      = 'Autoeficacia política';
  'sd_int_pol'   = 'Interés en política';
  
  'sd_pref_autor'   = 'Preferencias autoritarias';

  'sd_just_distrib'         = 'Justicia distributiva';
  
  'di_conf_inst_pol'         = 'Confianza en instituciones';
  'di_just_distrib'         = 'Justicia distributiva';
  'di_prac_acti_pol'         = 'Participación y actitudes políticas';
  'di_pref_autor'         = 'Preferencia por autoritarismo';
  'di_redes_sociales'         = 'Redes sociales';
  'di_seguridad_pub'         = 'Seguridad pública';
  'di_vinculos_territ'         = 'Vínculos territoriales'
  
")

# Verificar subdimensiones
sjmisc::frq(df_categ_jerarquica$subdimension)

# Crear dimensiones
df_categ_jerarquica$dimension <- 
  car::recode(df_categ_jerarquica$subdimension, "
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
sjmisc::frq(df_categ_jerarquica$dimension)

# Crear áreas
df_categ_jerarquica$area <- 
  car::recode(df_categ_jerarquica$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

df_categ_jerarquica <- df_categ_jerarquica %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

df_categ_jerarquica <- df_categ_jerarquica %>% 
  select(ola, valor, subdimension, dimension, area, prop, n)

save(df_categ_jerarquica, file = here ("data/bases-vis-elsoc/df_categ_jerarquica.RData"))

# ==============================================
# BASE 5: Base longitudinal con estructura jerárquica proporciones
# ==============================================
# 1. Define el vector con todas las variables que vas a modificar
todas_las_variables <- c(
  "in_seg_peleas_calle",
  "in_seg_asaltos",
  "in_seg_trafico_drogas",
  "in_seg_seguridad_sat",
  "in_seg_seguridad_perc",
  "in_bar_ideal",
  "in_bar_integracion",
  "in_bar_identidad",
  "in_bar_pertenencia",
  "in_bar_amigos",
  "in_bar_sociable",
  "in_bar_cordial",
  "in_bar_colaborador",
  "in_conf_inter_general",
  "in_conf_inter_altruismo",
  "in_prosoc_reunion_pub",
  "in_prosoc_voluntariado",
  "in_ayuda_prestar_dinero",
  "in_ayuda_trabajo",
  "in_ayuda_donar_dinero",
  "in_conf_inst_gobierno",
  "in_conf_inst_pp",
  "in_conf_inst_congreso",
  "in_part_firma_peticion",
  "in_part_asiste_marcha",
  "in_part_huelga",
  "in_autoef_voto_deber",
  "in_autoef_voto_influye",
  "in_autoef_voto_expresion",
  "in_intpol_interes_politica",
  "in_intpol_hablar_politica",
  "in_intpol_infopolitica_medios",
  "in_autor_gobierno_firme",
  "in_autor_mandatario_fuerte",
  "in_autor_vida_disciplinar",
  "in_just_pensiones",
  "in_just_educacion",
  "in_just_salud"
)


# 2. Recodifica todas las variables de una vez
db_categ <- db_long %>%
  mutate(across(all_of(todas_las_variables), ~case_when(
    .x >= 3 ~ "Alto",
    .x < 3  ~ "Bajo"
  ))) %>% 
  select(c(1:12, 76:83),all_of(todas_las_variables))

db_categ <- db_categ %>% 
  mutate_at(.vars = todas_las_variables, .funs = ~ as_factor(.))


df_categ_jerarquica <- db_categ %>%  # Aquí faltan los indicadores base
  select(ola, todas_las_variables) %>% 
  pivot_longer(
    cols = -ola,
    names_to = "variable",
    values_to = "valor"
  ) %>% 
  na.omit()

df_categ_jerarquica <- df_categ_jerarquica %>%
  group_by(ola, variable, valor) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(prop = n / sum(n))

df_categ_jerarquica <- df_categ_jerarquica %>%
  pivot_wider(
    names_from = variable,
    values_from = c(n, prop),
    values_fill = NA,
    names_glue = "{variable}_{.value}"
  )

# Crear base longitudinal con todos los indicadores
variables_indicadores <- df_categ_jerarquica %>% ungroup() %>% select(starts_with("in_")) %>% names()

df_categ_jerarquica <- df_categ_jerarquica %>%
  pivot_longer(
    cols = matches("^(in|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(in|di|sd)_(.+)_(n|prop)$"
  )

df_categ_jerarquica <- df_categ_jerarquica %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

df_categ_jerarquica <- df_categ_jerarquica %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
df_categ_jerarquica$subdimension <-NA
# Verificar subdimensiones
sjmisc::frq(df_categ_jerarquica$subdimension)

df_categ_jerarquica$subdimension <- 
  car::recode(df_categ_jerarquica$indicador, "
  'in_seg_peleas_calle'       = 'Seguridad objetiva';
  'in_seg_asaltos'            = 'Seguridad objetiva';
  'in_seg_trafico_drogas'     = 'Seguridad objetiva';
  
  'in_seg_seguridad_sat'      = 'Seguridad subjetiva';
  'in_seg_seguridad_perc'     = 'Seguridad subjetiva';
  
  'in_bar_ideal'              = 'Pertenencia al Barrio';
  'in_bar_integracion'        = 'Pertenencia al Barrio';
  'in_bar_identidad'          = 'Pertenencia al Barrio';
  'in_bar_pertenencia'        = 'Pertenencia al Barrio';
  
  'in_bar_amigos'             = 'Satisfacción con el barrio';
  'in_bar_sociable'           = 'Satisfacción con el barrio';
  'in_bar_cordial'            = 'Satisfacción con el barrio';
  'in_bar_colaborador'        = 'Satisfacción con el barrio';
  
  'in_conf_inter_general'     = 'Confianza interpersonal';
  'in_conf_inter_altruismo'   = 'Confianza interpersonal';
  
  'in_prosoc_reunion_pub'     = 'Comportamiento prosocial';
  'in_prosoc_voluntariado'    = 'Comportamiento prosocial';
  
  'in_ayuda_prestar_dinero'   = 'Ayuda económica';
  'in_ayuda_trabajo'          = 'Ayuda económica';
  'in_ayuda_donar_dinero'     = 'Ayuda económica';
  
  'in_conf_inst_gobierno'     = 'Confianza en instituciones políticas';
  'in_conf_inst_pp'           = 'Confianza en instituciones políticas';
  'in_conf_inst_congreso'     = 'Confianza en instituciones políticas';
  
  'in_part_firma_peticion'    = 'Participación política';
  'in_part_asiste_marcha'     = 'Participación política';
  'in_part_huelga'            = 'Participación política';
  
  'in_autoef_voto_deber'      = 'Autoeficacia política';
  'in_autoef_voto_influye'    = 'Autoeficacia política';
  'in_autoef_voto_expresion'  = 'Autoeficacia política';

  'in_intpol_interes_politica'   = 'Interés en política';
  'in_intpol_hablar_politica'    = 'Interés en política';
  'in_intpol_infopolitica_medios'= 'Interés en política';
  
  'in_autor_gobierno_firme'   = 'Preferencias autoritarias';
  'in_autor_mandatario_fuerte'= 'Preferencias autoritarias';
  'in_autor_vida_disciplinar' = 'Preferencias autoritarias';
  
  'in_just_pensiones'         = 'Justicia distributiva';
  'in_just_educacion'         = 'Justicia distributiva';
  'in_just_salud'             = 'Justicia distributiva'
")

# Verificar subdimensiones
sjmisc::frq(df_long_jerarquica$subdimension)

# Crear dimensiones
df_categ_jerarquica$dimension <- 
  car::recode(df_categ_jerarquica$subdimension, "
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
sjmisc::frq(df_categ_jerarquica$dimension)

# Crear áreas
df_categ_jerarquica$area <- 
  car::recode(df_categ_jerarquica$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")


df_categ_jerarquica <- df_categ_jerarquica %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

df_categ_jerarquica <- df_categ_jerarquica %>% 
  select(ola, valor, subdimension, dimension, area, prop, n)
df_categ_jerarquica_ranking <- df_categ_jerarquica
save(df_categ_jerarquica_ranking, file = here ("data/bases-vis-elsoc/df_categ_jerarquica_ranking.RData"))



