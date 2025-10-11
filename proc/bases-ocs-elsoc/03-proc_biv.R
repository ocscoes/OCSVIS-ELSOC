# 0. Identification ------------------------------------------------

# Title: Data preparation Bivariados/covariables
# Institution: OCS
# Responsible: Andreas Laffert
# Executive Summary: This script contains the code to data preparation for analysis of cohesion in VIS-ELSOC
# Date: Sep 30, 2025

# 1. Packages  -----------------------------------------------------

if (! require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               car,
               sjmisc,
               here,
               sjlabelled,
               sjPlot)
options(scipen=999)
rm(list = ls())

# 2. Data ----------------------------------------------------------

load(file = here ("data/bases-vis-elsoc/db_madre.RData"))
load(file = here ("data/bases-vis-elsoc/db_categ.RData"))

glimpse(db_madre)

# 3. Processing ----------------------------------------------------------

# 5. Bivariate BBDD -------------------------------------------------------

## By dimensions and subdimensions mean


# 5.1 Sexo

sexo_mean <- db_madre %>% 
  group_by(ola, sexo) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
    ) %>% 
  filter(!is.na(sexo))
  
sexo_mean 

# 5.2 Edad

edad_mean <- db_madre %>% 
  group_by(ola, edad_t) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(edad_t))

edad_mean 

# 5.3 Educ mean

cine_mean <- db_madre %>% 
  group_by(ola, cine) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(cine))

cine_mean 

educ_dic_mean <- db_madre %>% 
  group_by(ola, educ_dic) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(educ_dic))

educ_dic_mean 

# 5.4 Ingresos

ingreso_mean <- db_madre %>% 
  group_by(ola, grupo_ingreso) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(grupo_ingreso))

ingreso_mean 

ingresona_mean <- db_madre %>% 
  group_by(ola, grupo_ingreso1) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(grupo_ingreso1))

ingresona_mean 

# 5.5 Ideologia

ideologia_mean <- db_madre %>% 
  group_by(ola, ideologia) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(ideologia))

ideologia_mean 

# 5.6 Religion

religion_mean <- db_madre %>% 
  group_by(ola, religion) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(religion))

religion_mean 

# 5.7 Estado civil

estcivil_mean <- db_madre %>% 
  group_by(ola, estado_civil) %>% 
  summarise(
    across(
      .cols = starts_with(c("sd_", "di_", "ar_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(estado_civil))

estcivil_mean 

## By dimensions and subdimensions categorical

# 5.8 Sexo categ

sexo_categ <- db_categ %>% 
  select(ola, sexo, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, sexo, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, sexo, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(sexo))

sexo_categ


# 5.9 Edad categ

edad_categ <- db_categ %>% 
  select(ola, edad_t, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, edad_t, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, edad_t, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(edad_t))


edad_categ


# 5.9 Educ categ

cine_categ <- db_categ %>% 
  select(ola, cine, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, cine, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, cine, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(cine))


educ_dic_categ <- db_categ %>% 
  select(ola, educ_dic, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, educ_dic, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, educ_dic, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(educ_dic))


cine_categ
educ_dic_categ


# 5.10 Ingresos categ

ingreso_categ <- db_categ %>% 
  select(ola, grupo_ingreso, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, grupo_ingreso, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, grupo_ingreso, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(grupo_ingreso))

ingresona_categ <- db_categ %>% 
  select(ola, grupo_ingreso1, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, grupo_ingreso1, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, grupo_ingreso1, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(grupo_ingreso1))

ingreso_categ
ingresona_categ

# 5.11 Ideologia categ

ideologia_categ <- db_categ %>% 
  select(ola, ideologia, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, ideologia, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, ideologia, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(ideologia))

ideologia_categ

# 5.12 Religion categ

religion_categ <- db_categ %>% 
  select(ola, religion, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, religion, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, religion, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(religion))

religion_categ

# 5.13 Estado civil categ

estcivil_categ <- db_categ %>% 
  select(ola, estado_civil, starts_with(c("sd_", "di_", "ar_"))) %>% 
  pivot_longer(cols = starts_with(c("sd_", "di_", "ar_")),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, estado_civil, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, estado_civil, categoria),
              names_from = variable,
              values_from = c(n, prop),
              names_glue = "{variable}_{.value}") %>% 
  filter(!is.na(estado_civil))

estcivil_categ


# 6. Save and export ------------------------------------------------------

save(
  sexo_mean,
  edad_mean,
  cine_mean,
  educ_dic_mean,
  ingreso_mean,
  ingresona_mean,
  ideologia_mean,
  religion_mean,
  estcivil_mean,
  file = here("data/bases-vis-elsoc/bivariados_long_promedios.RData"))


save(
  sexo_categ,
  edad_categ,
  cine_categ,
  educ_dic_categ,
  ingreso_categ,
  ingresona_categ,
  ideologia_categ,
  religion_categ,
  estcivil_categ,
  file = here("data/bases-vis-elsoc/bivariados_long_categoricos.RData"))
