# 0. Identification ---------------------------------------------------

# Title: Data Analysis Longitudinal
# Institution: OCS
# Responsible: René Canales

# Executive Summary: This script contains the code to panel data analysis of cohesion index in VIS-ELSOC
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

load(file = here("data/bases-vis-elsoc/db_long.RData"))

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
