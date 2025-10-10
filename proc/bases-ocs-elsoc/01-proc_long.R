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

# Verificar la inversión de escala
cat("=== VERIFICACIÓN DE INVERSIÓN DE ESCALA ===\n")
cat("Ahora: 5=Nunca problemas (muy seguro), 1=Siempre problemas (muy inseguro)\n")
cat("Rango de valores después de inversión:\n")
cat("peleas_calle:", range(db$in_seg_peleas_calle, na.rm = TRUE), "\n")
cat("asaltos:", range(db$in_seg_asaltos, na.rm = TRUE), "\n")
cat("trafico_drogas:", range(db$in_seg_trafico_drogas, na.rm = TRUE), "\n")

# Verificar distribución por ola
cat("\nDistribución de peleas_calle por ola (después de inversión):\n")
print(table(db$ola, db$in_seg_peleas_calle, useNA = "ifany"))

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

# Verificar la inversión
cat("=== VERIFICACIÓN INVERSIÓN PREFERENCIAS AUTORITARIAS ===\n")
cat("Ahora: 5=Totalmente en desacuerdo con autoritarismo (más democrático)\n")
cat("       1=Totalmente de acuerdo con autoritarismo (más autoritario)\n")


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


# Verificar la inversión
cat("=== VERIFICACIÓN INVERSIÓN JUSTICIA DISTRIBUTIVA ===\n")
cat("Ahora: 5=Totalmente en desacuerdo con injusticia (más justo)\n")
cat("       1=Totalmente de acuerdo con injusticia (menos justo)\n")

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

# 3.4 Check BBDD

glimpse(db)

sjPlot::view_df(db,
                show.frq = T,show.values = T,show.na = T,show.prc = T, show.type = T)


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

db_madre <- db

save(db_madre, file = here ("data/bases-vis-elsoc/db_madre.RData"))

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
db_categ <- db %>%
  mutate(across(all_of(todas_las_variables), ~case_when(
    .x >= 3 ~ "Alto",
    .x < 3  ~ "Bajo"
  ))) %>% 
  select(1:18, all_of(todas_las_variables))

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

# ==========================================
# BASE 3 RECODE: Promedios por ola y tipo de muestra (sin NAs)
# ==========================================

db_categ_ola_muestra <- db_categ %>%
  select(ola, muestra, all_of(todas_las_variables)) %>% 
  pivot_longer(
    cols = -c(ola, muestra),
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
df_long_jerarquica <- db %>%
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

