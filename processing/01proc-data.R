#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation EDUMER Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Survey Wave 1 processing variables of interest          
# Date: 02-05-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled,
               sjmisc, 
               sjPlot,
               rio,
               car,
               summarytools,
               here)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

edumer_or <- rio::import(file = here("input", "data", "original", "300424_BDD_estudiantes.sav")) %>% 
  as_tibble()

glimpse(edumer_or)
names(edumer_or)

# 3. Processing -----------------------------------------------------------

# 3.1 Select ----

edumer <- edumer_or %>% 
  select(SbjNum, d3_def, d2, p20, p21_ano, p21_mes, p22_1, p24, p28, 
         aleatorio, tratamiento, control, check_atencion, 
         starts_with(c("p1_", "p2_", "p9_")), )

# 3.2 Filter: No

# 3.3 Recode and transform ----

# 3.3.1 Experimental desing variables

edumer %>% 
  select(aleatorio, tratamiento, control, check_atencion) %>% 
  frq(.)

edumer <- edumer %>% 
  mutate(aletario = factor(aleatorio,
                           levels = 1:2,
                           labels = c("Tratamiento", "Control")),
         across(.cols = c(tratamiento, control),
                .fns = ~ factor(., levels = 1:2, labels = c("Colegio Municipal", "Colegio Privado"))),
         check_comprension = case_when(tratamiento == "Colegio Privado" | control == "Colegio Privado" ~ 1,
                                       is.na(tratamiento) | is.na(control) ~ 0,
                                       TRUE ~ 0))


edumer %>% 
  select(aleatorio, tratamiento, control, check_atencion, check_comprension) %>% 
  frq(.) # check

# 3.3.2 Meritocracy variables

## Social level

edumer %>% 
  select(starts_with("p1_")) %>% 
  frq(.)

edumer <- edumer %>% 
  mutate(
    across(
      .cols = starts_with("p1_"),
      .fns = ~ factor(., levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")) %>% 
        set_na(., na = c(88,99), drop.levels = T)
      )
    )

edumer %>% 
  select(starts_with("p1_")) %>% 
  frq(.) # check


## School level

edumer %>% 
  select(starts_with("p2_")) %>% 
  frq(.)

edumer <- edumer %>% 
  mutate(
    across(
      .cols = starts_with("p2_"),
      .fns = ~ factor(., levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")) %>% 
        set_na(., na = c(88,99), drop.levels = T)
    )
  )

edumer %>% 
  select(starts_with("p2_")) %>% 
  frq(.) # check

# 3.3.3 Preferences and inequality

edumer %>% 
  select(starts_with("p9_")) %>% 
  frq(.)

edumer <- edumer %>% 
  mutate(
    across(
      .cols = starts_with("p9_"),
      .fns = ~ factor(., levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo")) %>% 
        set_na(., na = c(88,99), drop.levels = T)
    )
  )

edumer %>% 
  select(starts_with("p9_")) %>% 
  frq(.) # check

# 3.3.4 Demografic

# Sex
frq(edumer$p20)

edumer$p20 <- factor(edumer$p20, 
                     levels = 1:3, 
                     labels = c("Hombre", "Mujer", "Otro"))


# Age
frq(edumer$p21_ano)

edumer$age <- (2023-edumer$p21_ano)

# Ethnic
frq(edumer$p24)

edumer <- edumer %>% 
  mutate(p24 = factor(p24, levels = 1:5, labels = c("Mapuche", "Aimara","Rapa Nui", "Quechua", "No, ninguno anteriores")),
         p24 = set_na(p24, na = c(88,99), drop.levels = T))

# Religion

frq(edumer$p28)

edumer <- edumer %>% 
  mutate(p28 = factor(p28, 
                      levels = 1:8, 
                      labels = c("Católico", "Evangélico", "Protestante", 
                                 "Judío", "Creyente no adherente", "Ateo",
                                 "Agnóstico", "Ninguna")),
         p28 = set_na(p28, na = c(88,99), drop.levels = T))


# School dependency
frq(edumer$d2)

edumer <- edumer %>%
  mutate(d2 = case_when(
    d2 %in% c(2, 3, 4, 7, 8, 10, 11) ~ 1,
    d2 == 5 ~ 2,
    d2 == 6 ~ 3,
    TRUE ~ NA_integer_
  ))

edumer$d2 <- factor(edumer$d2, levels=c(1,2,3), labels=c("Colegio Particular Subvencionado", "Colegio Municipal","Colegio Privado"))

# Curse
frq(edumer$d3_def)
edumer$d3_def <- tolower(edumer$d3_def)
frq(edumer$d3_def)

edumer$d3_def <- car::recode(edumer$d3_def, recodes = c("'1° medio c' = 'Media';
                                                         '1a' = 'Media'; 
                                                         '1b' = 'Media'; 
                                                         '1c' = 'Media';
                                                         '2 ° medio c' = 'Media';
                                                         '2 b' = 'Media';
                                                         '2 medio' = 'Media';
                                                         '2 medio a' = 'Media';
                                                         '2 medio b' = 'Media';
                                                         '2 medio c' = 'Media';
                                                         '2 mediob' = 'Media';
                                                         '2° medio' = 'Media';
                                                         '2° medio a' = 'Media';
                                                         '2° medio b' = 'Media';
                                                         '2° medio c' = 'Media';
                                                         '2°m' = 'Media';
                                                         '2m°c' = 'Media';
                                                         '2°medio a' = 'Media';
                                                         '2°medio b' = 'Media';
                                                         '2°mediob' = 'Media';
                                                         '2a' = 'Media';
                                                         '2b' = 'Media';
                                                         '2c' = 'Media';
                                                         '2c (2°medio c)' = 'Media';
                                                         '2do medio' = 'Media';
                                                         '2dom a' = 'Media';
                                                         '2m' = 'Media';
                                                         '2m° c' = 'Media';
                                                         '2ma' = 'Media';
                                                         '2mb' = 'Media';
                                                         '2mc' = 'Media';
                                                         '2medio' = 'Media';
                                                         '2medio a' = 'Media';
                                                         '2medio b' = 'Media';
                                                         '2medioa' = 'Media';
                                                         '2medioc' = 'Media';
                                                         'ii b' = 'Media';
                                                         'segundo medio a' = 'Media';
                                                         'segundo medio b' = 'Media';
                                                         'sugundo medio a' = 'Media';
                                                         '6a' = 'Basica'; 
                                                         '6b' = 'Basica'; 
                                                         '6c' = 'Basica';
                                                         '7' = 'Basica';
                                                         '7 a' = 'Basica';
                                                         '7 basico' = 'Basica';
                                                         '7 básico' = 'Basica';
                                                         '7 básico a' = 'Basica';
                                                         '7.b' = 'Basica';
                                                         '7°b' = 'Basica';
                                                         '7a' = 'Basica';
                                                         '7b' = 'Basica';
                                                         '7basico' = 'Basica';
                                                         'septimo a' = 'Basica';
                                                         'séptimo a'  = 'Basica';
                                                         'septimo b' = 'Basica';
                                                         'séptimo b' = 'Basica';
                                                         'sexto a' = 'Basica'"))

frq(edumer$d3_def)

# 3.4 Labelled ----
set_label(edumer$SbjNum) <- "Id persona"
set_label(edumer$d3_def) <- "Nivel Curso"
set_label(edumer$d2) <- "Dependencia Escuela"
set_label(edumer$p20) <- "Género"
set_label(edumer$age) <- "Edad"
set_label(edumer$p24) <- "Etnia"
set_label(edumer$p28) <- "Religion"
set_label(edumer$aleatorio) <- "Identificador Asignación Aleatoria al Tratamiento"
set_label(edumer$tratamiento) <- "Comprensión del Tratamiento"
set_label(edumer$control) <- "Comprensión del Control"
set_label(edumer$check_atencion) <- "Check atención"
set_label(edumer$p1_1) <- "En Chile, las personas son recompensadas por su esfuerzo"
set_label(edumer$p1_2) <- "En Chile, las personas son recompensadas por su inteligencia y habilidad"
set_label(edumer$p1_3) <- "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida"
set_label(edumer$p1_4) <- "En Chile, quienes tienen buenos contactos les va mejor en la vida"
set_label(edumer$p1_5) <- "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos"
set_label(edumer$p1_6) <- "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento"
set_label(edumer$p1_7) <- "Está bien que quienes tienen padres ricos les vaya bien en la vida"
set_label(edumer$p1_8) <- "Está bien que quienes tienen buenos contactos les vaya bien en la vida"
set_label(edumer$p1_9) <- "En Chile, todas las personas tienen las mismas oportunidades para salir adelante"
set_label(edumer$p1_10) <- "En Chile, todas las personas obtienen lo que merecen"
set_label(edumer$p2_1) <- "En esta escuela, quienes se esfuerzan obtienen buenas notas"
set_label(edumer$p2_2) <- "En esta escuela, quienes son inteligentes obtienen buenas notas"
set_label(edumer$p2_3) <- "En esta escuela, los/as estudiantes obtienen las notas que merecen"
set_label(edumer$p9_1) <- "Las diferencias económicas entre ricos y pobres en Chile son demasiado grandes"
set_label(edumer$p9_2) <- "Es responsabilidad del gobierno reducir las diferencias económicas entre las personas con altos ingresos y aquellas con bajos ingresos"
set_label(edumer$p9_3) <- "Está bien que aquellos que puedan pagar más tengan mejor educación"
set_label(edumer$p9_4) <- "Está bien que aquellos que puedan pagar más tengan mejor acceso a salud"
set_label(edumer$p9_5) <- "Está bien que en Chile las personas con mayores ingresos puedan tener mejores pensiones que las personas de ingresos más bajos"
set_label(edumer$p9_6) <- "Está bien que las personas más inteligentes y/o talentosas ganen más dinero, aun cuando requieran esforzarse menos para ello"

# 4. Save and export -----------------------------------------------------------------

db <- edumer %>% 
  select(id = SbjNum, 
         sexo = p20,
         edad = age,
         etni = p24,
         religion = p28,
         nivel_curso = d3_def,
         dep_escuela = d2, 
         aleatorio,
         tratamiento, 
         control, 
         check_atencion,
         check_comprension,
         merit_esfuerzo = p1_1, 
         merit_talento = p1_2, 
         percep_padres_ricos = p1_3, 
         percep_buenos_contactos = p1_4, 
         recom_esfuerzo = p1_5, 
         recom_talento =  p1_6, 
         just_padres_ricos = p1_7, 
         just_buenos_contactos = p1_8, 
         igualdad_oportunidades = p1_9, 
         percep_merecimiento = p1_10, 
         escuela_esfuerzo = p2_1, 
         escuela_talento = p2_2,
         escuela_calificaciones = p2_3,
         percep_desigualdad = p9_1, 
         pref_redis = p9_2,
         just_edu = p9_3,
         just_salud = p9_4,
         just_pension = p9_5,
         just_talento = p9_6)

save(db, file = here("output", "db_proc.RData"))
