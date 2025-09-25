# 0. Identification ---------------------------------------------------

# Title: Data preparation Longitudinal
# Institution: OCS
# Responsible: René Canales

# Executive Summary: This script contains the code to data preparation for analysis of cohesion in VIS-ELSOC
# Date: Sep 23, 2025

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               car,
               sjmisc, 
               here,
               sjlabelled,
               SciViews,
               naniar,
               readxl,
               sjPlot)


options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))

# 3.2 Processing -----------------------------------------------------------

elsoc_long_2016_2023[elsoc_long_2016_2023 ==-999] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-888] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-777] <- NA
elsoc_long_2016_2023[elsoc_long_2016_2023 ==-666] <- NA

db <- elsoc_long_2016_2023 %>% 
  select(idencuesta, 
         ola,
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
         donar_dinero = c07_05, 
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

# 3.3 Index Creation ----

# COHESIÓN HORIZONTAL

#-----Redes-----

# comportamiento_prosocial

db %>% 
  group_by(ola) %>% 
  select(reunion_pub, voluntariado) %>% 
  frq()

db$comportamiento_prosocial <- rowMeans(db[, c("reunion_pub", "voluntariado")], na.rm = TRUE)

frq(db$comportamiento_prosocial)
# ayuda_economica

db %>% 
  group_by(ola) %>% 
  select(prestar_dinero, ayuda_trabajo) %>% 
  frq()

db$ayuda_economica <- rowMeans(db[, c("prestar_dinero", "ayuda_trabajo")], na.rm = TRUE)

frq(db$ayuda_economica)
# confianza_inter

db %>% 
  group_by(ola) %>% 
  select(confianza_gen, altruismo_gen) %>% 
  frq()

db$confianza_inter <- rowMeans(db[, c("confianza_gen", "altruismo_gen")], na.rm = TRUE)

# Redes Sociales
db %>% 
  group_by(ola) %>% 
  select(comportamiento_prosocial, ayuda_economica, confianza_inter) %>% 
  frq()

db$redes_sociales <- rowMeans(db[, c("comportamiento_prosocial", "ayuda_economica", "confianza_inter")], na.rm = TRUE)

#----Seguridad-----

frq(db$confianza_inter)
# seguridad_sub

db %>% 
  group_by(ola) %>% 
  select(seguridad_sat, seguridad_perc) %>% 
  frq()

db$seguridad_sub <- rowMeans(db[, c("seguridad_sat", "seguridad_perc")], na.rm = TRUE)

frq(db$seguridad_sub)
# seguridad_obj

db %>% 
  group_by(ola) %>% 
  select(peleas_calle, asaltos, trafico_drogas) %>% 
  frq()

db$seguridad_obj <- rowMeans(db[, c("peleas_calle", "asaltos", "trafico_drogas")], na.rm = TRUE)
db$seguridad_obj <- (round(db$seguridad_obj * 2) / 2)

frq(db$seguridad_obj)

# Seguridad Pública

db %>% 
  group_by(ola) %>% 
  select(seguridad_sub, seguridad_obj) %>% 
  frq()

db$seguridad_pub <- rowMeans(db[, c("seguridad_sub", "seguridad_obj")], na.rm = TRUE)

#-----Vínculos Territoriales-----

# sentido_pertenencia

db %>% 
  group_by(ola) %>% 
  select(barrio_ideal, barrio_integracion, barrio_identidad, barrio_pertenencia) %>% 
  frq()

db$sentido_pertenencia <- rowMeans(db[, c("barrio_ideal", "barrio_integracion", "barrio_identidad", "barrio_pertenencia")], na.rm = TRUE)
db$sentido_pertenencia <- (round(db$sentido_pertenencia * 2) / 2)

frq(db$sentido_pertenencia)
# satisfaccion_barrio
db %>% 
  group_by(ola) %>% 
  select(barrio_amigos, barrio_sociable, barrio_cordial, barrio_colaborador) %>% 
  frq()

db$satisfaccion_barrio <- rowMeans(db[, c("barrio_amigos", "barrio_sociable", "barrio_cordial", "barrio_colaborador")], na.rm = TRUE)
db$satisfaccion_barrio <- (round(db$satisfaccion_barrio * 2) / 2)

frq(db$satisfaccion_barrio)

# Vínculos Territoriales

db %>% 
  group_by(ola) %>% 
  select(sentido_pertenencia, satisfaccion_barrio) %>% 
  frq()

db$vinculos_territ <- rowMeans(db[, c("sentido_pertenencia", "satisfaccion_barrio")], na.rm = TRUE)

# COHESIÓN VERTICAL

#-----Confianza en Instituciones Políticas-----

db %>% 
  group_by(ola) %>% 
  select(conf_gobierno, conf_congreso, conf_pp) %>% 
  frq()

db$conf_inst_pol <- rowMeans(db[, c("conf_gobierno", "conf_congreso", "conf_pp")], na.rm = TRUE)

#-----Participación Política-----

db %>% 
  group_by(ola) %>% 
  select(firma_peticion, asiste_marcha, part_huelga, opinion_rrss) %>% 
  frq()

db$pp_politica <- rowMeans(db[, c("firma_peticion", "asiste_marcha", "part_huelga", "opinion_rrss")], na.rm = TRUE)


#-----Autoeficacia Política-----

db %>% 
  group_by(ola) %>% 
  select(voto_deber, voto_influye, voto_expresion) %>% 
  frq()

db$auto_efic <- rowMeans(db[, c("voto_deber", "voto_influye", "voto_expresion")], na.rm = TRUE)


#-----Interés en Política-----

db %>% 
  group_by(ola) %>% 
  select(interes_politica, hablar_politica, infopolitica_medios) %>% 
  frq()

db$int_pol <- rowMeans(db[, c("interes_politica", "hablar_politica", "infopolitica_medios")], na.rm = TRUE)


#-----Preferencias Autoritarias-----

db %>% 
  group_by(ola) %>% 
  select(gobierno_firme, mandatario_fuerte,vida_disciplinar) %>% 
  frq()

db$pref_autor <- rowMeans(db[, c("gobierno_firme", "mandatario_fuerte", "vida_disciplinar")], na.rm = TRUE)


#-----Justicia Distributiva-----

db %>% 
  group_by(ola) %>% 
  select(justicia_pensiones, justicia_educacion, justicia_salud) %>% 
  frq()

db$just_distrib <- rowMeans(db[, c("justicia_pensiones", "justicia_educacion", "justicia_salud")], na.rm = TRUE)

###COHESIÓN HORIZONTAL###
db %>% 
  group_by(ola) %>% 
  select(seguridad_sub, vinculos_territ, redes_sociales) %>% 
  frq()

db$coh_horiz <- rowMeans(db[, c("seguridad_sub", "vinculos_territ", "redes_sociales")], na.rm = TRUE)

###COHESION VERTICAL###
db %>% 
  group_by(ola) %>% 
  select(conf_inst_pol, pp_politica, auto_efic, int_pol, pref_autor, just_distrib) %>% 
  frq()

db$coh_vert <- rowMeans(db[, c("conf_inst_pol", "pp_politica", "auto_efic", "int_pol", "pref_autor", "just_distrib")], na.rm = TRUE)

###COHESION GENERAL###
db %>% 
  group_by(ola) %>% 
  select(coh_horiz, coh_vert) %>% 
  frq()

db$coh_gral <- rowMeans(db[, c("coh_horiz", "coh_vert")], na.rm = TRUE)

# 3.4. Means by wave----

# Promedios de todos los indicadores por ola

#---Tablas---

# Confianza en Instituciones
aggregate(cbind(conf_inst_pol, conf_gobierno, conf_congreso, conf_pp) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Participación Política
aggregate(cbind(pp_politica, firma_peticion, asiste_marcha, part_huelga, opinion_rrss) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Autoeficacia Política
aggregate(cbind(auto_efic, voto_deber, voto_influye, voto_expresion) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Interés en Política
aggregate(cbind(int_pol, interes_politica, hablar_politica, infopolitica_medios) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Preferencias Autoritarias
aggregate(cbind(pref_autor, gobierno_firme, mandatario_fuerte, vida_disciplinar) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Justicia Distributiva
aggregate(cbind(just_distrib, justicia_pensiones, justicia_educacion, justicia_salud) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Seguridad
aggregate(cbind(seguridad_pub, seguridad_obj, seguridad_sub) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Vínculos Territoriales
aggregate(cbind(vinculos_territ, sentido_pertenencia, satisfaccion_barrio) ~ ola, data = db, FUN = mean, na.rm = TRUE)

# Redes
aggregate(cbind(redes_sociales, comportamiento_prosocial, ayuda_economica, confianza_inter) ~ ola, data = db, FUN = mean, na.rm = TRUE)

#Cohesion
aggregate(cbind(coh_horiz, coh_vert) ~ ola, data = db, FUN = mean, na.rm = TRUE)

#---Gráfico----
# Confianza en Instituciones

conf_inst <- aggregate(cbind(conf_inst_pol, conf_gobierno, conf_congreso, conf_pp) ~ ola, 
                       data = db, FUN = mean, na.rm = TRUE)

conf_inst$indicador_confianza <- rowMeans(conf_inst[,-1], na.rm = TRUE)

matplot(conf_inst$ola, conf_inst[,c("conf_inst_pol", "conf_gobierno", "conf_congreso", "conf_pp")], 
        type = "l", xlab = "Ola", ylab = "Promedio",  main = "Confianza en Instituciones", col = c("black", "red", "green", "orange"), lty = 1:3, lwd = 2)

legend("topright", 
       legend = c("Confianza General", "Gobierno", "Congreso", "Partidos Políticos"), 
       col = c("black", "red", "green", "orange"), lty = 1:3, lwd = 2, cex = 0.8)

# Participación Política
part_pol <- aggregate(cbind(pp_politica, firma_peticion, asiste_marcha, part_huelga, opinion_rrss) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(part_pol$ola, part_pol[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Participación Política",
        col = c("black", "red", "green", "orange", "blue"), lty = 1:5, lwd = 2)
legend("topright", legend = c("pp_politica", "Firma Petición", "Asiste Marcha", "Part. Huelga", "Opinión RRSS"), 
       col = c("black", "red", "green", "orange", "blue"), lty = 1:5, lwd = 2, cex = 0.8)

# Autoeficacia Política
autoef_pol <- aggregate(cbind(auto_efic, voto_deber, voto_influye, voto_expresion) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(autoef_pol$ola, autoef_pol[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Autoeficacia Política",
        col = c("black", "red", "forestgreen", "orange"), lty = 1:4, lwd = 2)
legend("topright", legend = c("Autoeficacia General", "Voto Deber", "Voto Influye", "Voto Expresión"), 
       col = c("black", "red", "forestgreen", "orange"), lty = 1:4, lwd = 2, cex = 0.8)

# Interés en Política
interes_pol <- aggregate(cbind(int_pol, interes_politica, hablar_politica, infopolitica_medios) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(interes_pol$ola, interes_pol[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Interés en Política",
        col = c("black", "purple", "orange", "darkgreen"), lty = 1:4, lwd = 2)
legend("topright", legend = c("Interés General", "Interés Política", "Hablar Política", "Info Medios"), 
       col = c("black", "purple", "orange", "darkgreen"), lty = 1:4, lwd = 2, cex = 0.8)

# Preferencias Autoritarias
pref_aut <- aggregate(cbind(pref_autor, gobierno_firme, mandatario_fuerte, vida_disciplinar) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(pref_aut$ola, pref_aut[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Preferencias Autoritarias",
        col = c("black", "darkred", "navy", "darkgreen"), lty = 1:4, lwd = 2)
legend("topright", legend = c("Preferencias Autoritarias", "Gobierno Firme", "Mandatario Fuerte", "Vida Disciplinar"), 
       col = c("black", "darkred", "navy", "darkgreen"), lty = 1:4, lwd = 2, cex = 0.8)

# Justicia Distributiva
just_dist <- aggregate(cbind(just_distrib, justicia_pensiones, justicia_educacion, justicia_salud) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(just_dist$ola, just_dist[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Justicia Distributiva",
        col = c("black", "darkgreen", "steelblue", "darkred"), lty = 1:4, lwd = 2)
legend("topright", legend = c("Justicia Distributiva", "Justicia Pensiones", "Justicia Educación", "Justicia Salud"), 
       col = c("black", "darkgreen", "steelblue", "darkred"), lty = 1:4, lwd = 2, cex = 0.8)

# Seguridad (corregido el error en el nombre del objeto)
seguridad <- aggregate(cbind(seguridad_pub, seguridad_obj, seguridad_sub) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(seguridad$ola, seguridad[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Seguridad",
        col = c("black", "darkblue", "red"), lty = 1:3, lwd = 2)
legend("topright", legend = c("Seguridad Pública", "Seguridad Objetiva", "Seguridad Subjetiva"), 
       col = c("black", "darkblue", "red"), lty = 1:3, lwd = 2, cex = 0.8)

# Vínculos Territoriales
vinc_terr <- aggregate(cbind(vinculos_territ, sentido_pertenencia, satisfaccion_barrio) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(vinc_terr$ola, vinc_terr[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Vínculos Territoriales",
        col = c("black", "forestgreen", "orange"), lty = 1:3, lwd = 2)
legend("topright", legend = c("Vínculos Territoriales", "Sentido Pertenencia", "Satisfacción Barrio"), 
       col = c("black", "forestgreen", "orange"), lty = 1:3, lwd = 2, cex = 0.8)

# Redes
redes <- aggregate(cbind(redes_sociales, comportamiento_prosocial, ayuda_economica, confianza_inter) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(redes$ola, redes[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Redes",
        col = c("black", "purple", "darkorange", "darkgreen"), lty = 1:4, lwd = 2)
legend("topright", legend = c("Redes General", "Comportamiento Prosocial", "Ayuda Económica", "Confianza Inter"), 
       col = c("black", "purple", "darkorange", "darkgreen"), lty = 1:4, lwd = 2, cex = 0.8)

# Cohesion

## HORIZONTAL

horizontal <- aggregate(cbind(coh_horiz, seguridad_pub, vinculos_territ, redes_sociales) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(horizontal$ola, horizontal[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Cohesión Horizontal",
        col = c("black", "purple", "darkorange", "darkgreen"), lty = 1:4, lwd = 2)
legend("topright", legend = c("Cohesión General", "Seguridad", "Vínculos Territoriales", "Redes Sociales"), 
       col = c("black", "purple", "darkorange", "darkgreen"), lty = 1:4, lwd = 2, cex = 0.8)


## VERTICAL
vertical <- aggregate(cbind(coh_vert, conf_inst_pol, pp_politica, auto_efic, int_pol, pref_autor, just_distrib) ~ ola, data = db, FUN = mean, na.rm = TRUE)

print(vertical)
print(sapply(vertical[,-1], range, na.rm = TRUE))  # Ver rangos de cada variable

matplot(vertical$ola, vertical[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Cohesión Vertical",
        col = c("black", "purple", "darkorange", "darkred", "darkblue", "darkgreen", "gold"), 
        lty = 1:7, lwd = 2,
        ylim = range(vertical[,-1], na.rm = TRUE))  # Rango automático completo

legend("topright", 
       legend = c("Coh. Vertical", "Conf. Inst. Pol", "Part. Política", "Autoeficacia", "Interés Pol", "Pref. Autor", "Just. Distrib"), 
       col = c("black", "purple", "darkorange", "darkred", "darkblue", "darkgreen", "gold"), 
       lty = 1:7, lwd = 2, cex = 0.7)

## COHESION GENERAL

cohesion <- aggregate(cbind(coh_gral, coh_horiz, coh_vert) ~ ola, data = db, FUN = mean, na.rm = TRUE)
matplot(cohesion$ola, cohesion[,-1], type = "l", 
        xlab = "Ola", ylab = "Promedio", main = "Cohesión General",
        col = c("black", "darkorange", "darkgreen"), lty = 1:3, lwd = 2)
legend("topright", legend = c("Cohesión General", "Cohesión Horizontal", "Cohesión Vertical"), 
       col = c("black", "darkorange", "darkgreen"), lty = 1:3, lwd = 2, cex = 0.8)



