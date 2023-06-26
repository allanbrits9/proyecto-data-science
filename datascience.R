#0. LOADING PACKAGES & SETUP WORKING DIRECTORY####
install.packages("readxl")
install.packages("tidyverse")

#1.LOADING DATA####
library(readxl)
library(tidyverse)
file.choose()
ruta_riesgoagua <- "D:\\doall\\Downloads\\number-of-deaths-by-risk-factor AGUA BD definitiva.xlsx"
file.choose()
ruta_aguasanitizada <- "D:\\doall\\Downloads\\number-of-deaths-by-risk-factor AGUA BD definitiva.xlsx"
file.choose()
ruta_resistencia <- "D:\\doall\\Downloads\\ResistenciaAntimicrobiana.xlsx"
agua_sanitizada <- read_excel(ruta_aguasanitizada)
muerte_agua <- read_excel(ruta_riesgoagua)
resistencia_antimicrobiana <- read_excel(ruta_resistencia)

#2. DATA PRE PROCESSING#### (entran datos sucios y salen limpios listos para ser analizados)
agua_sanitizada_limpio <- na.omit(agua_sanitizada)
muerte_agua_limpio <- na.omit(muerte_agua)
resistenciamicrobiana <- na.omit(resistencia,antimicrobiana)

#Tabla de factores de riesgo
unsafe_water <- riesgo_agua$`Unsafe water source`
summary(unsafe_water)
sd(unsafe_water)
var(unsafe_water)
boxplot(unsafe_water)

unsafe_sanitation <- riesgo_agua$`Unsafe sanitation`
summary(unsafe_sanitation)
sd(unsafe_sanitation)
var(unsafe_sanitation)
boxplot(unsafe_sanitation)

access_handwashing <- riesgo_agua$`No access to handwashing facility`
summary(access_handwashing)
sd(access_handwashing)
var(access_handwashing)
boxplot(access_handwashing)

#Tabla agua y sanitización sin limpiar
improved_drinking_water <- agua_sanitizada$`Number with access to improved drinking water`
summary(improved_drinking_water)
sd(improved_drinking_water)
var(improved_drinking_water)
boxplot(improved_drinking_water)

basic_drinking_water <- agua_sanitizada$`Number with access to basic drinking water`
summary(basic_drinking_water)
sd(basic_drinking_water)
var(basic_drinking_water)
boxplot(basic_drinking_water)

limited_drinking_water <- agua_sanitizada$`Number with access to limited drinking water`
summary(limited_drinking_water)
sd(limited_drinking_water)
var(limited_drinking_water)
boxplot(limited_drinking_water)

unimproved_drinking_water <- agua_sanitizada$`Number with access to unimproved drinking water`
summary(unimproved_drinking_water)
sd(unimproved_drinking_water)
var(unimproved_drinking_water)
boxplot(unimproved_drinking_water)

noaccess_drinking_water <- agua_sanitizada$`Number with no access to drinking water`
summary(noaccess_drinking_water)
sd(noaccess_drinking_water)
var(noaccess_drinking_water)
boxplot(noaccess_drinking_water)

safely_drinking_water <- agua_sanitizada$`Number with access to safely managed drinking water`
summary(safely_drinking_water)
sd(safely_drinking_water)
var(safely_drinking_water)
boxplot(safely_drinking_water)

improved_sanitation <- agua_sanitizada$`Number with access to improved sanitation`
summary(improved_sanitation)
sd(improved_sanitation)
var(improved_sanitation)
boxplot(improved_sanitation)

basic_sanitation <- agua_sanitizada$`Number with access to basic sanitation services`
summary(basic_sanitation)
sd(basic_sanitation)
var(basic_sanitation)
boxplot(basic_sanitation)

limited_sanitation <- agua_sanitizada$`Number with access to limited sanitation services`
summary(limited_sanitation)
sd(limited_sanitation)
var(limited_sanitation)
boxplot(limited_sanitation)

unimproved_sanitation <- agua_sanitizada$`Number with access to unimproved sanitation facilities`
summary(unimproved_sanitation)
sd(unimproved_sanitation)
var(unimproved_sanitation)
boxplot(unimproved_sanitation)

open_defecation <- agua_sanitizada$`Number practicing open defecation (no sanitation facilities)`
summary(open_defecation)
sd(open_defecation)
var(open_defecation)
boxplot(open_defecation)

wastewater_sanitation <- agua_sanitizada$`Number with access to sanitation with off-site wastewater treatment`
summary(wastewater_sanitation)
sd(wastewater_sanitation)
var(wastewater_sanitation)
boxplot(wastewater_sanitation)

basic_handwashing <- agua_sanitizada$`Access to basic handwashing facilities`
summary(basic_handwashing)
sd(basic_handwashing)
var(basic_handwashing)
boxplot(basic_handwashing)

limited_handwashing <- agua_sanitizada$`Number with access to limited handwashing facilities`
summary(limited_handwashing)
sd(limited_handwashing)
var(limited_handwashing)
boxplot(limited_handwashing)

no_handwashing <- agua_sanitizada$`Number with no handwashing facilities`
summary(no_handwashing)
sd(no_handwashing)
var(no_handwashing)
boxplot(no_handwashing)


#3.DATA MODELING#### (DESCUBRIR PATRONES EN LOS DATOS CON DATA VIZ O CORRER MODELOS)

#Mortalidad con 
lambda <- mean(riesgo_agua_limpio$`Unsafe water source`)
dpois(riesgo_agua_limpio$`Unsafe sanitation`, lambda, log = FALSE)


#4.RESULTS#### (PRODUCCION DE TABLAS O FIGURAS QUE PRESENTAN LOS RESULTADOS FINALES DE TU MODEL)