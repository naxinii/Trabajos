
#Trabajo 03: R para Análisis Estadístico

#Asociación de variables y construcción de índices



#Antecedentes - Simce 2017
#Simce es parte de las evaluaciones que se aplican en Chile, definidas en el Plan de Evaluaciones Nacionales e Internacionales, 
#y corresponde a una medición estandarizada anual a todas y todos los estudiantes del país que cursan los niveles evaluados. (Para los fines de esta
#investigación, se tomarán los resultados de los estudiantes de 2° medio en 2017).
#Su objetivo es conocer los resultados educativos de los establecimientos, evaluando el logro de los contenidos y habilidades del Currículum Nacional,
#con pruebas en diferentes asignaturas o áreas de aprendizaje; y recogiendo información sobre el contexto educativo a través de cuestionarios que 
#responden directores(as), docentes, estudiantes, padres, madres y apoderados, de modo de analizar los resultados en forma integral.

#Parte 1

#Recursos de las prácticas y preparación de datos

#1. Librerías a utilizar

pacman::p_load(dplyr, 
               sjmisc, 
               sjPlot, 
               sjlabelled, 
               kableExtra, 
               GGally, 
               corrplot,
               tidyverse, 
               car, 
               haven,
               summarytools,
               psych)

rm(list=ls())       
options(scipen=999)

#2. Carga de la base de datos

#Para el trabajo 02 se realizó la preparación de los datos y se generaron 2 bases de datos (la 1ra incluye los casos NA y la 2da no) simce2m2017_total.RData y simce2m2017_final.Rdata, 
#en ambas se trabajó con las siguientes variables:


#VARIABLE 1:"cod_depe2" = Código de dependencia 3 categorías (Municipal(1); Particular subvencionado(2); Particular pagado(3))

#VARIABLE 2: "cod_grupo" = Código de grupo socioeconómico (Bajo(1);Medio bajo(2);Medio(3);Medio alto(4);Alto(5))

#VARIABLE 3: "cdm_2017" = Categoría de Desempeño 2017 (INSUFICIENTE, MEDIO-BAJO, MEDIO Y ALTO)
# Los establecimientos que tengan valores del Índice Final hasta el valor del percentil 12 (inclusive) de la distribución de este índice se clasificarán en la categoría Desempeño Insuficiente.
# Los establecimientos que presenten valores del Índice Final por sobre el valor del percentil 12 y hasta el percentil 35 (inclusive) se clasificarán en la categoría Desempeño Medio-Bajo.
# Los establecimientos que se sitúen por sobre el valor del percentil 35 y hasta el percentil 85 (inclusive) de la distribución del índice Final se clasificarán en la categoría Desempeño Medio.
# Los establecimientos con valores del Índice Final superiores al valor del percentil 85 de la distribución de este índice se clasificarán en la categoría Desempeño Alto.

#Para ver si podemos hacer el análisis con casos completos revisamos que los NA no sean más del 10%

load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_total.RData")
load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_final.RData")

dim(simce2m2017_total)
dim(simce2m2017_final)

2876-2765

111/2876*100

#Ya que sólo el 3.859527% del total son casos perdidos no supone problemas trabajar con la base simce2m2017_final.RData, pd: la preparación y tratamiento de 
#los casos perdidos con "listwise deletion" se encuentra en entre las líneas 203 - 215 y 260 - 276 del Script "Procesamiento(2)" del Trabajo 2


load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_final.RData")
dim(simce2m2017_final)
frq(simce2m2017_final)

#Convertir variables desde "factor" a "numeric"

simce2m2017_final$cod_grupo <- as.numeric(factor(simce2m2017_final$cod_grupo, levels = c("Bajo","Medio bajo","Medio","Medio alto","Alto")))

simce2m2017_final$cod_depe2 <- as.numeric(factor(simce2m2017_final$cod_depe2, levels =c("Municipal", "Particular subvencionado","Particular pagado")))

simce2m2017_final$cdm_2017 <- as.numeric(factor(simce2m2017_final$cdm_2017, levels =c("INSUFICIENTE","MEDIO-BAJO","MEDIO","ALTO")))

frq(simce2m2017_final)

#Recodificar para que la escala parta de "0" (Práctico 7)

simce2m2017_final$cod_grupo <- recode(simce2m2017_final$cod_grupo,"1=0; 2=1; 3=2; 4=3; 5=4")

simce2m2017_final$cod_depe2 <- recode(simce2m2017_final$cod_depe2,"1=0; 2=1; 3=2")

simce2m2017_final$cdm_2017 <- recode(simce2m2017_final$cdm_2017,"1=0; 2=1; 3=2; 4=3")

frq(simce2m2017_final)


simce2m2017_finalnum <- simce2m2017_final
save(simce2m2017_finalnum,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_finalnum.RData")

load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_finalnum.RData")
dim(simce2m2017_finalnum)
frq(simce2m2017_finalnum)

#Re-agregar etiquetas y nombrar

simce2m2017_finalnum$cod_grupo <- set_labels(simce2m2017_finalnum$cod_grupo,
                                             labels = c("Bajo"=0,
                                                        "Medio bajo"=1,
                                                        "Medio"=2,
                                                        "Medio alto"=3,
                                                        "Alto"=4))

simce2m2017_finalnum$cod_grupo <- set_label(x = simce2m2017_finalnum$cod_grupo,label = "GSE")

simce2m2017_finalnum$cod_depe2 <- set_labels(simce2m2017_finalnum$cod_depe2,
                                             labels = c("Municipal"=0,
                                                        "Particular subvencionado"=1,
                                                        "Particular pagado"=2))

simce2m2017_finalnum$cod_depe2 <- set_label(x = simce2m2017_finalnum$cod_depe2,label = "Código dependencia")

simce2m2017_finalnum$cdm_2017 <- set_labels(simce2m2017_finalnum$cdm_2017,
                                            labels = c("INSUFICIENTE"=0,
                                                       "MEDIO-BAJO"=1,
                                                       "MEDIO"=2,
                                                       "ALTO"=3))

simce2m2017_finalnum$cdm_2017 <- set_label(x = simce2m2017_finalnum$cdm_2017,label = "Categoría desempeño")

frq(simce2m2017_finalnum)
save(simce2m2017_finalnum,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_finalnum.RData")

load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_finalnum.RData")
frq(simce2m2017_finalnum)

sjmisc::descr(simce2m2017_finalnum,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")



#Parte 2

#Matrices de correlación

#Opción 1: cor

M <- cor(simce2m2017_finalnum, use = "complete.obs")
M

#Opción 2: tab_ corr

sjPlot::tab_corr(simce2m2017_finalnum, 
                 triangle = "lower")

#Opción 3: corrplot.mixed

corrplot.mixed(M)

#Opción 4: ggpairs

ggpairs(simce2m2017_finalnum)

#Opción 5: scatterplot

sjPlot::plot_scatter(simce2m2017_finalnum, cdm_2017, cod_depe2)

sjPlot::plot_scatter(simce2m2017_finalnum, cdm_2017, cod_grupo)

sjPlot::plot_scatter(simce2m2017_finalnum, cod_grupo, cod_depe2)

#Construcción de Escalas

load("~/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_finalnum.RData")
frq(simce2m2017_finalnum)
dim(simce2m2017_finalnum)

#Utilizaremos las mismas variables, sumado a que la base "simce2m2017_finalnum" ya se encuentra sin casos NA

#Estimar correlación
cor(simce2m2017_finalnum)

#Estimar consistencia interna: Alfa de Chronbach

psych::alpha(simce2m2017_finalnum)

psych::alpha(dplyr::select(simce2m2017_finalnum, cdm_2017, cod_depe2, cod_grupo))

simce2m2017_finalnum2 <- simce2m2017_finalnum %>% 
  rowwise() %>% 
  mutate(segregación_escolar = sum(cdm_2017, cod_depe2, cod_grupo))
summary(simce2m2017_finalnum2$segregación_escolar)

save(simce2m2017_finalnum2,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Trabajos/input/data-proc/simce2m2017_finalnum2.RData")

ggplot(simce2m2017_finalnum2, aes(x = segregación_escolar)) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Segregación Escolar") +
  ylab("Cantidad")














































































































































































































































































































