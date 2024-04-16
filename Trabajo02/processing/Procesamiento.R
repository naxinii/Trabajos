#Trabajo 02: R para Análisis Estadístico

#Preparación de datos

#Antecedentes - Simce 2017
#Simce es parte de las evaluaciones que se aplican en Chile, definidas en el Plan de Evaluaciones Nacionales e Internacionales, 
#y corresponde a una medición estandarizada anual a todas y todos los estudiantes del país que cursan los niveles evaluados.
#Su objetivo es conocer los resultados educativos de los establecimientos, evaluando el logro de los contenidos y habilidades del Currículum Nacional,
#con pruebas en diferentes asignaturas o áreas de aprendizaje; y recogiendo información sobre el contexto educativo a través de cuestionarios que 
#responden directores(as), docentes, estudiantes, padres, madres y apoderados, de modo de analizar los resultados en forma integral.

#1. Librerías a utilizar

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#2. Carga de la base de datos

rm(list=ls())       
options(scipen=999) 

library(readxl)
cdm_2017 <- read_excel("C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-orig/cdm_2017.xlsx")
idps2m2017_rbd <- read_excel("C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-orig/idps2m2017_rbd.xlsx")
simce2m2017_rbd <- read_excel("C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-orig/simce2m2017_rbd.xlsx")

dim(cdm_2017)
dim(idps2m2017_rbd)
dim(simce2m2017_rbd)
View(cdm_2017)
View(idps2m2017_rbd)
View(simce2m2017_rbd)

#3. Selección de Variables a utilizar

#Para este análisis consideré relevante las siguientes variables, según corresponda

#De la base "cdm_2017"
 # "rbd" = Rol base de datos del establecimiento
 # "cdm_2017" = Categoría de Desempeño 2017 (ALTO, MEDIO, MEDIO-BAJO e INSUFICIENTE)

find_var(data = cdm_2017, "rbd")
find_var(data = cdm_2017, "cdm_2017")

proc_cdm_2017 <- cdm_2017 %>% select(rbd,cdm_2017)

names(proc_cdm_2017)
sjlabelled::get_label(proc_cdm_2017)

dim(proc_cdm_2017)
View(cdm_2017)

save(proc_cdm_2017,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-proc/proc_cdm_2017.RData")

#De la base "idps2m2017_rbd"
 # "rbd" = Rol base de datos del establecimiento
 # "cod_depe2" = Código de dependencia 3 categorías (Municipal(1); Particular subvencionado(2); Particular pagado(3))

find_var(data = idps2m2017_rbd, "rbd")
find_var(data = idps2m2017_rbd, "cod_depe2")

proc_idps2m2017_rbd <- idps2m2017_rbd %>% select(rbd,cod_depe2)

names(proc_idps2m2017_rbd)
sjlabelled::get_label(proc_idps2m2017_rbd)

dim(proc_idps2m2017_rbd)
View(proc_idps2m2017_rbd)

save(proc_idps2m2017_rbd,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-proc/proc_idps2m2017_rbd.RData")

#De la base "simce2m2017_rbd"
 # "rbd" = Rol base de datos del establecimiento
 # "cod_grupo" = Código de grupo socioeconómico (Bajo(1);Medio bajo(2);Medio(3);Medio alto(4);Alto(5))

find_var(data = simce2m2017_rbd, "rbd")
find_var(data = simce2m2017_rbd, "cod_grupo")

proc_simce2m2017_rbd <- simce2m2017_rbd %>% select(rbd,cod_grupo)

names(proc_simce2m2017_rbd)
sjlabelled::get_label(proc_simce2m2017_rbd)

dim(proc_simce2m2017_rbd)
View(proc_simce2m2017_rbd)

save(proc_simce2m2017_rbd,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-proc/proc_simce2m2017_rbd.RData")

#Unir por "rbd"

data <- merge(proc_cdm_2017,proc_idps2m2017_rbd, by="rbd")
simce2m2017_total <- merge (data, proc_simce2m2017_rbd, by="rbd")

dim(simce2m2017_total)
View(simce2m2017_total)

save(simce2m2017_total,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-proc/simce2m2017_total.RData")

load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/Trabajo02/input/data-proc/simce2m2017_total.RData")

#4. Procesamiento de Variables

# Variable 1: "rbd" = Rol base de datos del establecimiento

#a. Descriptivo

frq(simce2m2017_total$rbd)

#b. Recodificación - No aplica

#c. Etiquetado - No aplica

#Variable 2:"cod_depe2" = Código de dependencia 3 categorías (Municipal(1); Particular subvencionado(2); Particular pagado(3))

#a. Descriptivo

frq(simce2m2017_total$cod_depe2)

#b. Recodificación

simce2m2017_total$cod_depe2 <- recode(simce2m2017_total$cod_depe2, "Municipal=1; Particular pagado=3; Particular subvencionado=2")

#c. Etiquetado



#Variable 3: "cod_grupo" = Código de grupo socioeconómico (Bajo(1);Medio bajo(2);Medio(3);Medio alto(4);Alto(5))

#a. Descriptivo

frq(simce2m2017_total$cod_grupo)

#b. Recodificación



#c. Etiquetado



#Variable 4: "cdm_2017" = Categoría de Desempeño 2017 (ALTO, MEDIO, MEDIO-BAJO e INSUFICIENTE)

#a. Descriptivo

frq(simce2m2017_total$cdm_2017)

#b. Recodificación



#c. Etiquetado




