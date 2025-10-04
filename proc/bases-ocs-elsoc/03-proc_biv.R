# 0. Identification ------------------------------------------------

# Title: Data preparation Bivariados
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

load(file = here ("data/bases-vis-elsoc/db_long.RData"))

glimpse(db)

# 3. Processing ----------------------------------------------------------

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

# Compute income quintiles
db_long <- db_long %>% 
  group_by(ola) %>% 
  mutate(quintil = ntile(ing_pc, 5)) %>% 
  ungroup()

db_long$quintil <- 
  factor(db_long$quintil,
         levels = c(1, 2, 3, 4, 5),
         labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')) # Quintiles as factors

#reverse quintile, reference level is the highest quintile
#db$quintil <- forcats::fct_rev(db$quintil)

db_long$quintil <- 
  sjlabelled::set_label(x = db_long$quintil,
                        label = "Quintil de ingresos por hogar per cápita")  

sjmisc::frq(db_long$quintil)

#include new quintile category with missing cases
db_long$quintil1<-
  car::recode(db_long$quintil, 
              "'Q1'='Q1';'Q2'= 'Q2';'Q3'='Q3';'Q4'='Q4';'Q5'='Q5'; NA='QNA'")

#db$quintil1 <- factor(db$quintil1, c("Q1","Q2","Q3","Q4","Q5","QNA"))

db_long$quintil1 <- 
  sjlabelled::set_label(x = db_long$quintil1,
                        label = "Quintil de ingresos por hogar per cápita (NA)") 
sjmisc::frq(db_long$quintil1)

frq(db_long$ola) #ok

# 4. BBDD final and save -------------------------------------------------

db_long <- db_long %>% 
  select(-c(nhogar1,
            m46_nhogar,
            m54,
            m30,
            m30b,
            m30_rec,
            m30b_rec,
            m29,
            m29_rec,
            m29_imp,
            ipc,
            n_hogar,
            n_hogar_r))

# db_long promedios

save(db_long, file = here ("data/bases-vis-elsoc/db_long_bivariados.RData"))

# db_long categ

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
db_long_categ <- db_long %>%
  mutate(across(all_of(todas_las_variables), ~case_when(
    .x >= 3 ~ "Alto",
    .x < 3  ~ "Bajo"
  ))) 

db_long_categ <- db_long_categ %>% 
  mutate_at(.vars = todas_las_variables, .funs = ~ as_factor(.))

# 3. (Opcional) Revisa el resultado en una de las variables
frq(db_long_categ$seguridad_sub)

save(db_long_categ, file = here ("data/bases-vis-elsoc/db_long_bivariados_categ.RData"))

# 5. Bivariate BBDD -------------------------------------------------------

## By dimensions and subdimensions mean

variables <- c(
  "seguridad_sub", "seguridad_obj", "seguridad_pub",
  "sentido_pertenencia", "satisfaccion_barrio", "vinculos_territ",
  "comportamiento_prosocial", "ayuda_economica", "confianza_inter", "redes_sociales",
  "conf_inst_pol",
  "pp_politica", "auto_efic", "int_pol", "prac_acti_pol",
  "pref_autor",
  "justicia_pensiones", "justicia_educacion", "justicia_salud", "just_distrib", 
  "coh_horiz", "coh_vert", "coh_gral"
)

# 5.1 Sexo

sexo_mean <- db_long %>% 
  group_by(ola, sexo) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
    ) %>% 
  filter(!is.na(sexo))
  
sexo_mean 

# 5.2 Edad

edad_mean <- db_long %>% 
  group_by(ola, edad_t) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(edad_t))

edad_mean 

# 5.3 Educ mean

cine_mean <- db_long %>% 
  group_by(ola, cine) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(cine))

cine_mean 

educ_dic_mean <- db_long %>% 
  group_by(ola, educ_dic) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(educ_dic))

educ_dic_mean 

# 5.4 Ingresos

quintil_mean <- db_long %>% 
  group_by(ola, quintil) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(quintil))

quintil_mean 

quintilna_mean <- db_long %>% 
  group_by(ola, quintil1) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(quintil1))

quintilna_mean

# 5.5 Ideologia

ideologia_mean <- db_long %>% 
  group_by(ola, ideologia) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(ideologia))

ideologia_mean 

# 5.6 Religion

religion_mean <- db_long %>% 
  group_by(ola, religion) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(religion))

religion_mean 

# 5.7 Estado civil

estcivil_mean <- db_long %>% 
  group_by(ola, estado_civil) %>% 
  summarise(
    across(
      .cols = all_of(variables),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>% 
  filter(!is.na(estado_civil))

estcivil_mean 

## By dimensions and subdimensions categorical

# 5.8 Sexo categ

sexo_categ <- db_long_categ %>% 
  select(ola, sexo, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, sexo, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, sexo, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(sexo))

sexo_categ


# 5.9 Edad categ

edad_categ <- db_long_categ %>% 
  select(ola, edad_t, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, edad_t, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, edad_t, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(edad_t))


edad_categ


# 5.9 Educ categ

cine_categ <- db_long_categ %>% 
  select(ola, cine, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, cine, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, cine, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(cine))


educ_dic_categ <- db_long_categ %>% 
  select(ola, educ_dic, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, educ_dic, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, educ_dic, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(educ_dic))


cine_categ
educ_dic_categ


# 5.10 Ingresos categ

quintil_categ <- db_long_categ %>% 
  select(ola, quintil, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, quintil, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, quintil, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(quintil))


quintilna_categ <- db_long_categ %>% 
  select(ola, quintil1, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, quintil1, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, quintil1, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(quintil1))


quintil_categ
quintilna_categ

# 5.11 Ideologia categ

ideologia_categ <- db_long_categ %>% 
  select(ola, ideologia, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, ideologia, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, ideologia, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(ideologia))

ideologia_categ

# 5.12 Religion categ

religion_categ <- db_long_categ %>% 
  select(ola, religion, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, religion, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, religion, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(religion))

religion_categ

# 5.13 Estado civil categ

estcivil_categ <- db_long_categ %>% 
  select(ola, estado_civil, all_of(variables)) %>% 
  pivot_longer(cols = all_of(variables),
               names_to = "variable", values_to = "categoria") %>% 
  group_by(ola, estado_civil, variable, categoria) %>%
  summarise(n = n(), .groups = "drop_last") %>% 
  filter(!is.na(categoria)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(ola, estado_civil, categoria),
              names_from = variable,
              values_from = c(n, prop)) %>% 
  filter(!is.na(estado_civil))

estcivil_categ


# 6. Save and export ------------------------------------------------------

save(
  sexo_mean,
  edad_mean,
  cine_mean,
  educ_dic_mean,
  quintil_mean,
  quintilna_mean,
  ideologia_mean,
  religion_mean,
  estcivil_mean,
  file = here("data/bases-vis-elsoc/bivariados_long_promedios.RData"))


save(
  sexo_categ,
  edad_categ,
  cine_categ,
  educ_dic_categ,
  quintil_categ,
  quintilna_categ,
  ideologia_categ,
  religion_categ,
  estcivil_categ,
  file = here("data/bases-vis-elsoc/bivariados_long_categoricos.RData"))



