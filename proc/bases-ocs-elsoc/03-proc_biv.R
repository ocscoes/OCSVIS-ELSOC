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


sexo_categ <- sexo_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

sexo_categ <- sexo_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

sexo_categ <- sexo_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
sexo_categ$subdimension <- 
  car::recode(sexo_categ$indicador, "
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
sjmisc::frq(sexo_categ$subdimension)

# Crear dimensiones
sexo_categ$dimension <- 
  car::recode(sexo_categ$subdimension, "
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
sjmisc::frq(sexo_categ$dimension)

# Crear áreas
sexo_categ$area <- 
  car::recode(sexo_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

sexo_categ <- sexo_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

sexo_categ <- sexo_categ %>% 
  select(ola, sexo, categoria, subdimension, dimension, area, prop, n)

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



edad_categ <- edad_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

edad_categ <- edad_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

edad_categ <- edad_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
edad_categ$subdimension <- 
  car::recode(edad_categ$indicador, "
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
sjmisc::frq(edad_categ$subdimension)

# Crear dimensiones
edad_categ$dimension <- 
  car::recode(edad_categ$subdimension, "
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
sjmisc::frq(edad_categ$dimension)

# Crear áreas
edad_categ$area <- 
  car::recode(edad_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

edad_categ <- edad_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

edad_categ <- edad_categ %>% 
  select(ola, edad_t, categoria, subdimension, dimension, area, prop, n)

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


cine_categ <- cine_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

cine_categ <- cine_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

cine_categ <- cine_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
cine_categ$subdimension <- 
  car::recode(cine_categ$indicador, "
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
sjmisc::frq(cine_categ$subdimension)

# Crear dimensiones
cine_categ$dimension <- 
  car::recode(cine_categ$subdimension, "
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
sjmisc::frq(cine_categ$dimension)

# Crear áreas
cine_categ$area <- 
  car::recode(cine_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

cine_categ <- cine_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

cine_categ <- cine_categ %>% 
  select(ola, cine, categoria, subdimension, dimension, area, prop, n)


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


educ_dic_categ <- educ_dic_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

educ_dic_categ <- educ_dic_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

educ_dic_categ <- educ_dic_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
educ_dic_categ$subdimension <- 
  car::recode(educ_dic_categ$indicador, "
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
sjmisc::frq(educ_dic_categ$subdimension)

# Crear dimensiones
educ_dic_categ$dimension <- 
  car::recode(educ_dic_categ$subdimension, "
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
sjmisc::frq(educ_dic_categ$dimension)

# Crear áreas
educ_dic_categ$area <- 
  car::recode(educ_dic_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

educ_dic_categ <- educ_dic_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

educ_dic_categ <- educ_dic_categ %>% 
  select(ola, educ_dic, categoria, subdimension, dimension, area, prop, n)

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


ingreso_categ <- ingreso_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

ingreso_categ <- ingreso_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

ingreso_categ <- ingreso_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
ingreso_categ$subdimension <- 
  car::recode(ingreso_categ$indicador, "
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
sjmisc::frq(ingreso_categ$subdimension)

# Crear dimensiones
ingreso_categ$dimension <- 
  car::recode(ingreso_categ$subdimension, "
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
sjmisc::frq(ingreso_categ$dimension)

# Crear áreas
ingreso_categ$area <- 
  car::recode(ingreso_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

ingreso_categ <- ingreso_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

ingreso_categ <- ingreso_categ %>% 
  select(ola, grupo_ingreso, categoria, subdimension, dimension, area, prop, n)


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


ingresona_categ <- ingresona_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

ingresona_categ <- ingresona_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

ingresona_categ <- ingresona_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
ingresona_categ$subdimension <- 
  car::recode(ingresona_categ$indicador, "
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
sjmisc::frq(ingresona_categ$subdimension)

# Crear dimensiones
ingresona_categ$dimension <- 
  car::recode(ingresona_categ$subdimension, "
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
sjmisc::frq(ingresona_categ$dimension)

# Crear áreas
ingresona_categ$area <- 
  car::recode(ingresona_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

ingresona_categ <- ingresona_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

ingresona_categ <- ingresona_categ %>% 
  select(ola, grupo_ingreso1, categoria, subdimension, dimension, area, prop, n)

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


ideologia_categ <- ideologia_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

ideologia_categ <- ideologia_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

ideologia_categ <- ideologia_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
ideologia_categ$subdimension <- 
  car::recode(ideologia_categ$indicador, "
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
sjmisc::frq(ideologia_categ$subdimension)

# Crear dimensiones
ideologia_categ$dimension <- 
  car::recode(ideologia_categ$subdimension, "
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
sjmisc::frq(ideologia_categ$dimension)

# Crear áreas
ideologia_categ$area <- 
  car::recode(ideologia_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

ideologia_categ <- ideologia_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

ideologia_categ <- ideologia_categ %>% 
  select(ola, ideologia, categoria, subdimension, dimension, area, prop, n)

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


religion_categ <- religion_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

religion_categ <- religion_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

religion_categ <- religion_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
religion_categ$subdimension <- 
  car::recode(religion_categ$indicador, "
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
sjmisc::frq(religion_categ$subdimension)

# Crear dimensiones
religion_categ$dimension <- 
  car::recode(religion_categ$subdimension, "
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
sjmisc::frq(religion_categ$dimension)

# Crear áreas
religion_categ$area <- 
  car::recode(religion_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

religion_categ <- religion_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

religion_categ <- religion_categ %>% 
  select(ola, religion, categoria, subdimension, dimension, area, prop, n)


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


estcivil_categ <- estcivil_categ %>% 
  pivot_longer(
    cols = matches("^(ar|di|sd)_"),                   
    names_to = c("prefijo", "indicador", ".value"),   
    names_pattern = "^(ar|di|sd)_(.+)_(n|prop)$"
  )

estcivil_categ <- estcivil_categ %>% 
  mutate(indicador = paste0(prefijo, "_", indicador))

estcivil_categ <- estcivil_categ %>% 
  filter(prefijo!="ar") %>% 
  select(-c(prefijo))

# Crear subdimensiones
estcivil_categ$subdimension <- 
  car::recode(estcivil_categ$indicador, "
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
sjmisc::frq(estcivil_categ$subdimension)

# Crear dimensiones
estcivil_categ$dimension <- 
  car::recode(estcivil_categ$subdimension, "
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
sjmisc::frq(estcivil_categ$dimension)

# Crear áreas
estcivil_categ$area <- 
  car::recode(estcivil_categ$dimension, "
            'Seguridad pública'  = 'Area horizontal';
            'Vínculos territoriales' = 'Area horizontal';
            'Redes sociales' = 'Area horizontal';
            
            'Confianza en instituciones' = 'Area vertical';            
            'Participación y actitudes políticas' = 'Area vertical';
            'Preferencia por autoritarismo' = 'Area vertical';
            'Justicia distributiva' = 'Area vertical'
            ")

estcivil_categ <- estcivil_categ %>% 
  mutate(dimension = if_else(dimension == subdimension, NA, dimension))

estcivil_categ <- estcivil_categ %>% 
  select(ola, estado_civil, categoria, subdimension, dimension, area, prop, n)


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
