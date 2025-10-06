# 0. Identification ------------------------------------------------

# Title: Data preparation Ranking
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
               sjPlot,
               ggdist)


options(scipen=999)
rm(list = ls())

# 2. Data ----------------------------------------------------------

load(file = here("data/bases-vis-elsoc/db_promedios_ola.Rdata"))


my_pretty_theme <- theme_ggdist(base_size = 12) +
  theme(
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0),
    plot.caption = element_text(size = 13),
    
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, face = "bold"),
    axis.text.y  = element_text(size = 16),
    
    legend.title = element_text(size = 16, face = "bold"),
    legend.text  = element_text(size = 14),
    legend.box.background = element_rect(color = "grey20", fill = "white", linewidth = 0.5),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.margin         = margin(t = 6, r = 8, b = 6, l = 8),
    legend.box.margin     = margin(t = 6, r = 6, b = 6, l = 6),
    
    legend.position  = "top",
    legend.direction = "horizontal",
    strip.text = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# 3. Graphs Ranking ------------------------------------------------
# Etiquetas y colores para Vínculos Territoriales
etiquetas_territ <- c(
  barrio_ideal = "Barrio ideal",
  barrio_integracion = "Integración barrial",
  barrio_identidad = "Identidad barrial",
  barrio_pertenencia = "Pertenencia barrial",
  barrio_amigos = "Amigos en el barrio",
  barrio_sociable = "Sociabilidad barrial",
  sentido_pertenencia = "Sentido de pertenencia",
  satisfaccion_barrio = "Satisfacción con barrio",
  vinculos_territ = "Vínculos territoriales"
)

colores_territ <- c("#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E", "#B8860B", 
                   "#824293", "#487FD3", "#7ABA21")

# Etiquetas y colores para Redes Sociales
etiquetas_redes <- c(
  confianza_gen = "Confianza generalizada",
  altruismo_gen = "Altruismo generalizado",
  reunion_pub = "Reuniones públicas",
  voluntariado = "Voluntariado",
  prestar_dinero = "Prestar dinero",
  ayuda_trabajo = "Ayuda en trabajo",
  comportamiento_prosocial = "Comportamiento prosocial",
  ayuda_economica = "Ayuda económica",
  confianza_inter = "Confianza interpersonal"
)

colores_redes <- c("#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E", "#B8860B",
                  "#824293", "#487FD3", "#7ABA21")

# Etiquetas y colores para Confianza en Instituciones
etiquetas_conf_inst <- c(
  conf_gobierno = "Confianza en gobierno",
  conf_pp = "Confianza en partidos políticos",
  conf_judicial = "Confianza en sistema judicial",
  conf_congreso = "Confianza en congreso",
  conf_inst_pol = "Confianza en inst. políticas"
)

colores_conf_inst <- c("#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E")

# Etiquetas y colores para Prácticas y Actitudes Políticas
etiquetas_pract_pol <- c(
  asiste_marcha = "Asistir a marchas",
  part_huelga = "Participar en huelgas",
  opinion_rrss = "Opinión en RRSS",
  voto_deber = "Voto como deber",
  voto_influye = "Voto influye",
  voto_expresion = "Voto como expresión",
  interes_politica = "Interés en política",
  hablar_politica = "Hablar de política",
  infopolitica_medios = "Info política medios",
  pp_politica = "Participación política",
  auto_efic = "Autoeficacia política",
  int_pol = "Interés político"
)

colores_pract_pol <- c("#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E", "#B8860B",
                      "#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E", "#B8860B")

# Etiquetas y colores para Preferencias por Sistema Político
etiquetas_pref_sist <- c(
  gobierno_firme = "Gobierno firme",
  mandatario_fuerte = "Mandatario fuerte",
  vida_disciplinar = "Vida disciplinaria",
  sat_democracia = "Satisfacción democracia",
  pref_autor = "Preferencia autoritaria"
)

colores_pref_sist <- c("#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E")

# Etiquetas y colores para Justicia Distributiva
etiquetas_just_dist <- c(
  justicia_pensiones = "Justicia en pensiones",
  justicia_educacion = "Justicia en educación",
  justicia_salud = "Justicia en salud",
  just_distrib = "Justicia distributiva"
)

colores_just_dist <- c("#824293", "#487FD3", "#7ABA21", "#F9913D")
# Seguridad Pública por Ola
# Variables de seguridad pública
vars_seguridad <- c("seguridad_sat", "seguridad_perc", "peleas_calle", 
                    "asaltos", "trafico_drogas")


# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

etiquetas <- c(
  seguridad_sat = "Satisfacción con la seguridad",
  seguridad_perc = "Percepción de seguridad",
  peleas_calle = "Peleas en la calle",
  asaltos = "Asaltos",
  trafico_drogas = "Tráfico de drogas"
)

colores <- c("#824293", "#487FD3", "#7ABA21", "#F9913D", "#FF3E4E", "#B8860B")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme
  

# HASTA AQUI HAY UN EJEMPLO DE GRAFICO DE RANKING WITHIN SUBDIMENSION, ES DECIR
# COMPARANDO INDICADORES EN UNA OLA RESPECTIVA. CON ESTA BASE, HACER UN FOR 
# PARA LAS DEMAS OLAS REPLICANDO EL MISMO GRAFICO, PEDIRSELO A CHATGPT, PARA
# MINIMIZAR ERRORES Y HACER EL CODIGO MAS EFIICIENTE

# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2021 ==========
datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2021",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2022 ==========
datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2023 ==========
datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas[x], width = 15)) +
  scale_fill_manual(values = colores) +
  labs(
    title = "Seguridad Pública - Ola 2023",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# Vinculos Territoriales por Ola
vars_territ <- c("barrio_ideal", "barrio_integracion", "barrio_identidad", "barrio_pertenencia", "barrio_amigos", "barrio_sociable", 
                 "sentido_pertenencia", "satisfaccion_barrio")


# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_territ[x], width = 15)) +
  scale_fill_manual(values = colores_territ) +
  labs(
    title = "Vínculos Territoriales - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_territ[x], width = 15)) +
  scale_fill_manual(values = colores_territ) +
  labs(
    title = "Vínculos Territoriales - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_territ[x], width = 15)) +
  scale_fill_manual(values = colores_territ) +
  labs(
    title = "Vínculos Territoriales - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_territ[x], width = 15)) +
  scale_fill_manual(values = colores_territ) +
  labs(
    title = "Vínculos Territoriales - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2021 ==========

#---No hay datos de Variables de Vínculos Territoriales en la Ola 2021---#

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_territ[x], width = 15)) +
  scale_fill_manual(values = colores_territ) +
  labs(
    title = "Vínculos Territoriales - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2023 ==========

datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_territ[x], width = 15)) +
  scale_fill_manual(values = colores_territ) +
  labs(
    title = "Vínculos Territoriales - Ola 2023",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# Redes Sociales por Ola

vars_redes <- c("confianza_gen", "altruismo_gen", "reunion_pub", "voluntariado", "prestar_dinero", "ayuda_trabajo",
                "comportamiento_prosocial")

# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2021 ==========
datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2021",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2022 ==========
datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2023 ==========
datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_redes[x], width = 15)) +
  scale_fill_manual(values = colores_redes) +
  labs(
    title = "Redes Sociales - Ola 2023",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# Confianza en Instituciones por Ola

vars_conf_inst <- c("conf_gobierno", "conf_pp", "conf_judicial", "conf_congreso")

# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2021 ==========
datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2021",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2022 ==========
datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2023 ==========
datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_conf_inst[x], width = 15)) +
  scale_fill_manual(values = colores_conf_inst) +
  labs(
    title = "Confianza en Instituciones - Ola 2023",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# Prácticas y Actitudes Políticas por Ola

var_pract_pol <- c("asiste_marcha", "part_huelga", "opinion_rrss", "voto_deber", "voto_influye",
"voto_expresion", "interes_politica", "hablar_politica", "infopolitica_medios", "pp_politica",
"auto_efic", "int_pol")

# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2017 ==========

datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2018 ==========

datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2019 ==========

datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2021 ==========

datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2021",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2023 ==========

datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pract_pol[x], width = 15)) +
  scale_fill_manual(values = colores_pract_pol) +
  labs(
    title = "Prácticas y Actitudes Políticas - Ola 2023",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# Preferencias por Sistema Político por Ola

var_pref_sist <- c("gobierno_firme", "mandatario_fuerte", "vida_disciplinar", "sat_democracia", "pref_autor")

# ========== GRÁFICO OLA 2016 ==========

datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pref_sist[x], width = 15)) +
  scale_fill_manual(values = colores_pref_sist) +
  labs(
    title = "Preferencias por Sistema Político - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2017 ==========

datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pref_sist[x], width = 15)) +
  scale_fill_manual(values = colores_pref_sist) +
  labs(
    title = "Preferencias por Sistema Político - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2018 ==========

datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pref_sist[x], width = 15)) +
  scale_fill_manual(values = colores_pref_sist) +
  labs(
    title = "Preferencias por Sistema Político - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2019 ==========

datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pref_sist[x], width = 15)) +
  scale_fill_manual(values = colores_pref_sist) +
  labs(
    title = "Preferencias por Sistema Político - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2021 ==========

datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pref_sist[x], width = 15)) +
  scale_fill_manual(values = colores_pref_sist) +
  labs(
    title = "Preferencias por Sistema Político - Ola 2021",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_pref_sist[x], width = 15)) +
  scale_fill_manual(values = colores_pref_sist) +
  labs(
    title = "Preferencias por Sistema Político - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# Justicia Distributiva por Ola

var_just_dist <- c("justicia_pensiones", "justicia_educacion", "justicia_salud", "just_distrib")

# ========== GRÁFICO OLA 2016 ==========

datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_just_dist[x], width = 15)) +
  scale_fill_manual(values = colores_just_dist) +
  labs(
    title = "Justicia Distributiva - Ola 2016",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2017 ==========

datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_just_dist[x], width = 15)) +
  scale_fill_manual(values = colores_just_dist) +
  labs(
    title = "Justicia Distributiva - Ola 2017",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2018 ==========

datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_just_dist[x], width = 15)) +
  scale_fill_manual(values = colores_just_dist) +
  labs(
    title = "Justicia Distributiva - Ola 2018",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2019 ==========

datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_just_dist[x], width = 15)) +
  scale_fill_manual(values = colores_just_dist) +
  labs(
    title = "Justicia Distributiva - Ola 2019",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


# ========== GRÁFICO OLA 2021 ==========

# No se presentan datos de Justicia Distributiva para el 2021

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_just_dist[x], width = 15)) +
  scale_fill_manual(values = colores_just_dist) +
  labs(
    title = "Justicia Distributiva - Ola 2022",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme

# ========== GRÁFICO OLA 2023 ==========

datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, group = variable)) +
  geom_col(aes(fill = variable), show.legend = F, alpha = 0.8) +
  scale_y_continuous(limits = c(0,5),
                     n.breaks = 10) +
  scale_x_discrete(labels = function(x) str_wrap(etiquetas_just_dist[x], width = 15)) +
  scale_fill_manual(values = colores_just_dist) +
  labs(
    title = "Justicia Distributiva - Ola 2023",
    x = NULL,
    y = "Promedio",
    caption = "Fuente: Elaboración propia en base a datos agrupados ELSOC"
  ) +
  my_pretty_theme


