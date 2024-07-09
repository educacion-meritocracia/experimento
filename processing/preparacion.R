# 0. Preparacion encuesta estudiantes ola 1. Se realiza un procesamiento a 9 variables 
      #referidas al experimento, merito, meritocracia en la escuela y justificacion de la desigualdad 

# 1. cargar librerias ---------------------------------------------------------
install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer)
library(haven)

# 2. cargar bbdd --------------------------------------------------------------
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica


datos_estudiantes <- read_sav("input/data/original/300424_BDD_estudiantes.sav")


# 3. seleccionar variables ----------------------------------------------------

#p1_1 (en chile, las personas son recompensadas por su esfuerzo)
#p1_2 (en chile, las personas son recompensadas por su inteligencia y habilidad)
#p1_10 (En Chile, todas las personas obtienen lo que merecen)
#p2_1 (quienes se esfuerzan obtienen buenas notas)
#p2_2 (en esta esceula, queines son inteligentes obtienen buenas notas)
#p2_3 (en esta escuela, los/as estudiantes obtienen las notas que se merecen)
#p9_3 (esta bien que aquellos que puedan pagar mas tengan mejor educación)
#p9_4 (esta bien que aquellos que puedan pagar mas tengan mejor acceso a salud)
#p9_5 (esta bien que en Chile las personas con mayores ingresos puedan tener 
        #mejores pensiones que las personas de ingresos mas bajos) 
#d3_def (en que curso estas)
#p26 (nivel de estudios de la madre)
#p27 (nivel de estudios del padre)
#p30 (cantidad de libros en el hogar)

frq(datos_estudiantes$tratamiento)

proc_datos_estudiantes <- datos_estudiantes %>% select(aleatorio,
                                                       p1_1, p1_2, p1_10,
                                                       p2_1, p2_2, p2_3, 
                                                       p9_3, p9_4, p9_5, 
                                                       d3_def, p26, p27, 
                                                       p30, p20, check_atencion, tratamiento, control, d2)
#renombrar 
proc_datos_estudiantes <- proc_datos_estudiantes %>% rename(tratamiento, control,
                                                            soc_esfuerzo = p1_1,
                                                            soc_talento = p1_2,
                                                            soc_merit = p1_10,
                                                            school_esfuerzo = p2_1,
                                                            school_talento = p2_2,
                                                            school_merito = p2_3,
                                                            just_educ = p9_3,
                                                            just_salud = p9_4,
                                                            just_pensiones = p9_5,
                                                            curso_estudiante = d3_def,
                                                            ne_madre = p26,
                                                            ne_padre = p27, 
                                                            libros_hogar = p30,
                                                            genero = p20,
                                                            check_tratamiento = tratamiento,
                                                            check_control = control,
                                                            school_dependencia = d2)                                                             

# Comprobar
names(proc_datos_estudiantes)

# 4. procesamiento de variables -----------------------------------------------

#ordenar por variable (9)

## aleatorio ----

get_label(proc_datos_estudiantes$aleatorio)

### a. descriptivo basico ----
frq(proc_datos_estudiantes$aleatorio) #no tiene etiquetas y no presenta casos perdidos

### b. etiquetamiento ----
proc_datos_estudiantes$aleatorio <- set_labels(proc_datos_estudiantes$aleatorio,
                             labels=c( "Tratamiento"= 1,
                                       "Control"= 2))

### c. recodificacion ----
proc_datos_estudiantes$aleatorio <- factor(proc_datos_estudiantes$aleatorio, 
                             levels=c(1,2),
                             labels=c("Tratamiento","Control"))

summary(proc_datos_estudiantes$aleatorio) #confirmar

## soc_esfuerzo  ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$soc_esfuerzo) #buen sentido. Etiquetda.Casos perdidos:
#88 no sabe tiene 2 casos y 99 preferiria no responder 0 casos. 

### b. recodificacion ----
proc_datos_estudiantes$soc_esfuerzo <- recode(proc_datos_estudiantes$soc_esfuerzo, "c(88,99)=NA")

### c. etiqutamiento ----
proc_datos_estudiantes$soc_esfuerzo <- set_label(x = proc_datos_estudiantes$soc_esfuerzo,label = 
                                                   "En Chile, las personas son recompensadas por su esfuerzo")

get_label(proc_datos_estudiantes$soc_esfuerzo)

## soc_talento ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$soc_talento) #buen sentido. Etiquetada. Casos perdidos: 
#88 no sabe 7 casos, 99 preferiria no responder 1 caso 

### b. recodificacion ----
proc_datos_estudiantes$soc_talento <- recode(proc_datos_estudiantes$soc_talento, "c(88,99)=NA")

## soc_merit ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$soc_merit) #buen sentido. Etiquetada. Casos perdidos: 
#88 no sabe 2 casos, 99 preferiria no responder 2 casos 

### b. recodificacion ----
proc_datos_estudiantes$soc_merit <- recode(proc_datos_estudiantes$soc_merit, "c(88,99)=NA")

## school_esfuerzo ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_esfuerzo) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 0 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos_estudiantes$school_esfuerzo <- recode(proc_datos_estudiantes$school_esfuerzo, "c(88,99)=NA")

## school_talento ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_talento) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 0 casos, 99 preferiria no responder 1 caso. 

### b. recodificacion ----
proc_datos_estudiantes$school_talento <- recode(proc_datos_estudiantes$school_talento, "c(88,99)=NA")

## school_merito ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_merito) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 3 casos.

### b. recodificacion ----
proc_datos_estudiantes$school_merito <- recode(proc_datos_estudiantes$school_merito, "c(88,99)=NA")

## just_educ ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$just_educ) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 0 casos.

### b. recodificacion ----
proc_datos_estudiantes$just_educ <- recode(proc_datos_estudiantes$just_educ, "c(88,99)=NA")

## just_salud ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$just_salud) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 3 casos, 99 preferiria no responder 2 casos

### b. recodificacion ----
proc_datos_estudiantes$just_salud <- recode(proc_datos_estudiantes$just_salud, "c(88,99)=NA")

## just_pensiones ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$just_pensiones) #buen sentido. Etiquetada. Casos perdidos: 
  #88 no sabe 2 casos, 99 preferiria no responder 1 caso

### b. recodificacion ----
proc_datos_estudiantes$just_pensiones <- recode(proc_datos_estudiantes$just_pensiones, "c(88,99)=NA")

## curso_estudiante ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$curso_estudiante) #no tiene NA 
proc_datos_estudiantes$curso_estudiante <- tolower(proc_datos_estudiantes$curso_estudiante)
frq(proc_datos_estudiantes$curso_estudiante)

### b. recodificacion ----
proc_datos_estudiantes$curso_estudiante <- car::recode(proc_datos_estudiantes$curso_estudiante, recodes = c("'1° medio c' = 'Media';
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
                                                                                                            'sexto a' = 'Basica'
                                                                                                            "))

frq(proc_datos_estudiantes$curso_estudiante)
## genero ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$genero) #muestra con una mayoria de muejeres 48.52% y 
  #categoria otro: 4.52%. No tiene casos perdidos

### b. recodificacion ----
proc_datos_estudiantes$genero <- factor(proc_datos_estudiantes$genero, 
                                           levels=c(1,2,3),
                                           labels=c("Hombre","Mujer","Otro"))

## check_tratamiento ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$check_tratamiento) #colegio privado 39.13%. Casos perdidos: 
  #concentra la mayoria de las respuestas 47.83% (275 casos)

### b. recodificacion ---- 
proc_datos_estudiantes$check_tratamiento <- factor(proc_datos_estudiantes$check_tratamiento, 
                                           levels=c(1,2),
                                           labels=c("Colegio Municipal","Colegio Privado"))

## check_control ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$check_control) #colegio privado 38.43%. Casos perdidos: 
  #concentra la mayoria de las respuestas 52.17% (300 casos)

### b. recodificacion ----
proc_datos_estudiantes$check_control <- factor(proc_datos_estudiantes$check_control, 
                                           levels=c(1,2),
                                           labels=c("Colegio Municipal","Colegio Privado"))

### c. otros ajustes ----

#variable check_comprension
proc_datos_estudiantes <- proc_datos_estudiantes %>% 
  mutate(check_comprension = case_when(
    check_tratamiento == "Colegio Privado" | check_control == "Colegio Privado" ~ 1,
    is.na(check_tratamiento) | is.na(check_control) ~ 0,
    TRUE ~ 0
  ))

frq(proc_datos_estudiantes$check_comprension)

## check_atencion ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$check_atencion) #en desacuerdo 88.17% (507 casos). 
  #no presenta casos perdidos

## school_dependencia ----

### a. descriptivo basico ----
frq(proc_datos_estudiantes$school_dependencia) #particular subvencionado 2, 3, 4, 7, 8, 10, 11
                                    #Municipal 5 
                                     #Privado  6
                                      
#10  Instituto del Puerto, San Antonio -> particular subvencionado  
#11  Liceo Santa Teresa de Los Andes -> particular subvencionado 
#12 tambien particular subvencionado

### b. recodificacion ----
proc_datos_estudiantes <- proc_datos_estudiantes %>%
  mutate(school_dependencia = case_when(
    school_dependencia %in% c(2, 3, 4, 7, 8, 10, 11) ~ 1,
    school_dependencia == 5 ~ 2,
    #    school_dependencia == 6 ~ 3,
    TRUE ~ NA_integer_
  ))

proc_datos_estudiantes$school_dependencia <- factor(proc_datos_estudiantes$school_dependencia, 
                                                    levels=c(1,2),
                                                    labels=c("Colegio Particular Subvencionado", "Colegio Municipal"))

frq(proc_datos_estudiantes$school_dependencia)

######## educacion padres----
frq(proc_datos_estudiantes$ne_padre)
frq(proc_datos_estudiantes$ne_madre)

# 5. base procesada -----------------------------------------------------------
proc_datos_estudiantes <-as.data.frame(proc_datos_estudiantes)
stargazer(proc_datos_estudiantes, type="text")

save(proc_datos_estudiantes,file = "input/data/proc/es_ola1.RData")
