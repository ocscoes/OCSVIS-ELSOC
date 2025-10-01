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
               naniar,
               sjPlot,
               psych,
               dplyr)


options(scipen=999)
rm(list = ls())

# 2. Data ----------------------------------------------------------

load(file = here("data/bases-vis-elsoc/db_promedios_ola.Rdata"))

# 3. Graphs Ranking ------------------------------------------------

# Seguridad Pública por Ola
# Variables de seguridad pública
vars_seguridad <- c("seguridad_sat", "seguridad_perc", "peleas_calle", 
                    "asaltos", "trafico_drogas", "seguridad_sub", "seguridad_obj")


# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2019",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2021 ==========
datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2021",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2022 ==========
datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2023 ==========
datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_seguridad)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Seguridad Pública - Ola 2023",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# Vinculos Territoriales por Ola
vars_territ <- c("barrio_ideal", "barrio_integracion", "barrio_identidad", "barrio_pertenencia", "barrio_amigos", "barrio_sociable", 
                 "sentido_pertenencia", "satisfaccion_barrio", "vinculos_territ")


# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Vinculos Territoriales - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Vinculos Territoriales - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Vinculos Territoriales - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# # ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Vinculos Territoriales - Ola 2019",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2021 ==========

#---No hay datos de Variables de Vínculos Territoriales en la Ola 2021---#

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Vinculos Territoriales - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2022 ==========

datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_territ)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Vinculos Territoriales - Ola 2023",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# Redes Sociales por Ola

vars_redes <- c("confianza_gen", "altruismo_gen", "reunion_pub", "voluntariado", "prestar_dinero", "ayuda_trabajo",
                "comportamiento_prosocial", "ayuda_economica", "confianza_inter")

# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2019",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2021 ==========
datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2021",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2022 ==========
datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2023 ==========
datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_redes)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Redes Sociales - Ola 2023",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# Confianza en Instituciones por Ola

vars_conf_inst <- c("conf_gobierno", "conf_pp", "conf_judicial", "conf_congreso", "conf_inst_pol")

# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2017 ==========
datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2018 ==========
datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2019 ==========
datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2019",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2021 ==========
datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2021",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2022 ==========
datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2023 ==========
datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(vars_conf_inst)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2023",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# Prácticas y Actitudes Políticas por Ola

var_pract_pol <- c("asiste_marcha", "part_huelga", "opinion_rrss", "voto_deber", "voto_influye",
"voto_expresion", "interes_politica", "hablar_politica", "infopolitica_medios", "pp_politica",
"auto_efic", "int_pol")

# ========== GRÁFICO OLA 2016 ==========
datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Práctticas y Actitudes Políticas - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2017 ==========

datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2018 ==========

datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2017 ==========

datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2019",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2021 ==========

datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2021",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2023 ==========

datos_2023 <- db_promedios_ola %>%
  filter(ola == "2023") %>%
  select(all_of(var_pract_pol)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2023, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Confianza en Instituciones - Ola 2023",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# Preferencias por Sistema Político por Ola

var_pref_sist <- c("gobierno_firme", "mandatario_fuerte", "vida_disciplinar", "sat_democracia", "pref_autor")

# ========== GRÁFICO OLA 2016 ==========

datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Preferencias por Sistema Político - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2016 ==========

datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Preferencias por Sistema Político - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2018 ==========

datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Preferencias por Sistema Político - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2019 ==========

datos_2019 <- db_promedios_ola %>%
  filter(ola == "2019") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2019, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Preferencias por Sistema Político - Ola 2019",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2021 ==========

datos_2021 <- db_promedios_ola %>%
  filter(ola == "2021") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2021, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Preferencias por Sistema Político - Ola 2021",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(var_pref_sist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Preferencias por Sistema Político - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# Justicia Distributiva por Ola

var_just_dist <- c("justicia_pensiones", "justicia_educacion", "justicia_salud", "just_distrib")

# ========== GRÁFICO OLA 2016 ==========

datos_2016 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Justicia Distributiva - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2017 ==========

datos_2017 <- db_promedios_ola %>%
  filter(ola == "2017") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2017, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Justicia Distributiva - Ola 2017",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2018 ==========

datos_2018 <- db_promedios_ola %>%
  filter(ola == "2018") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2018, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Justicia Distributiva - Ola 2018",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2019 ==========

datos_2019 <- db_promedios_ola %>%
  filter(ola == "2016") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2016, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Justicia Distributiva - Ola 2016",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


# ========== GRÁFICO OLA 2021 ==========

# No se presentan datos de Justicia Distributiva para el 2021

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Justicia Distributiva - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )

# ========== GRÁFICO OLA 2022 ==========

datos_2022 <- db_promedios_ola %>%
  filter(ola == "2022") %>%
  select(all_of(var_just_dist)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "promedio")

ggplot(datos_2022, aes(x = reorder(variable, promedio), y = promedio, fill = promedio)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  scale_fill_gradient(low = "#FEE5D9", high = "#A50F15") +
  labs(
    title = "Justicia Distributiva - Ola 2022",
    x = NULL,
    y = "Promedio",
    fill = "Promedio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10)
  )


