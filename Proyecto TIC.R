#~~~~~~~~~~~~~~~~~~~#
# Proyecto Docentes #
#~~~~~~~~~~~~~~~~~~~#

library(readxl)
library(tidyverse)
library(hrbrthemes)

setwd("C:/Users/user/Desktop/Universidad/Muestreo/Docentes - TIC/Docentes")

#============================================================================================================

#~~~~~~~~~~#
# Encuesta #
#~~~~~~~~~~#

# Cargamos los datos de la encuesta y seleccionamos los mas relevantes.

setwd("C:/Users/user/Desktop/Universidad/Muestreo/Docentes - TIC/Docentes")

doce_en <- read_excel("Docentes.xlsx")
doce_en <- doce_en[ , c(13, seq(0,9), 11, 12, seq(14, 17), seq(20, 171))]
doce_names <- colnames(doce_en) # lo utilizaremos para el diccionario 

# Codificamos los nombres de las columnas.

for(i in 1:168) {
  colnames(doce_en)[i]= paste("do", i, sep = "")
}

# Reducimos los NA del id
doce_en$do1 <- ifelse(doce_en$do1=="NA", doce_en$do13, doce_en$do1)

# glimpse(doce_en)
# table(doce_en$do49)
# table(doce_en$do50)
# table(doce_en$do61)
# table(doce_en$do85)
# table(doce_en$do124)
# table(doce_en$do134)

#============================================================================================================

# Codificamos las respuestas de las variables.

doce_sub <- doce_en

for(i in 1:168) {
  doce_sub[i]  <- ifelse(doce_sub[i]=="No", 0, 
                         ifelse(doce_sub[i]=="Sí"|doce_sub[i]=="1- No tengo acceso a internet"|doce_sub[i]=="1- A diario"|doce_sub[i]=="1- En su casa"|doce_sub[i]=="1- En desacuerdo"|doce_sub[i]=="1- Mejorado bastante"|doce_sub[i]=="1- Nada seguro", 
                                1, 
                                ifelse(doce_sub[i]=="2- Menos de 1 hora"|doce_sub[i]=="2- Al menos una vez por semana"|doce_sub[i]=="2- En la casa de un amigo"|doce_sub[i]=="2- Algo de acuerdo"|doce_sub[i]=="2- Mejorado"|doce_sub[i]=="2- Algo seguro", 
                                       2, 
                                       ifelse(doce_sub[i]=="3- De 1 a 3 horas"|doce_sub[i]=="3- Rara vez"|doce_sub[i]=="3- En la casa de un pariente"|doce_sub[i]=="3- De acuerdo"|doce_sub[i]=="3- Permanecido igual"|doce_sub[i]=="3- Muy seguro", 
                                              3, 
                                              ifelse(doce_sub[i]=="4- De 4 a 6 horas"|doce_sub[i]=="4- Ninguna vez"|doce_sub[i]=="4- En la institución educativa"|doce_sub[i]=="4- Muy de acuerdo"|doce_sub[i]=="4- Empeorado", 
                                                     4, 
                                                     ifelse(doce_sub[i]=="5- De 7 a 9 horas"|doce_sub[i]=="5- En algún lugar público (shopping, café, biblioteca, plaza, etc.)"|doce_sub[i]=="5- Empeorado bastante", 
                                                            5, 
                                                            ifelse(doce_sub[i]=="6- Más de 9 horas"| doce_sub[i]=="Otro", 
                                                                   6, NA)))))))
}

# Agregamos las respuestas que no estan codificadas.

for(i in c(seq(1:15), seq(24,29), 33, 41, 48, 60, 62, 69, 84, 106, 123, 168)) {
  doce_sub[i] = doce_en[i]
}

#============================================================================================================

#-------------#
# Diccionario #
#-------------#

# Creamos el diccionario de los datos de la encuesta.

dic <- doce_en %>% 
  summary.default %>% as.data.frame %>% 
  dplyr::group_by(Var1) %>%  
  tidyr::spread(key = Var2, value = Freq) %>% 
  select(-Class, -Length) %>% 
  mutate(Names = 0, 
         R_0= 0,
         R_1= 0,
         R_2= 0,
         R_3= 0,
         R_4= 0,
         R_5= 0,
         R_6= 0,
         R_NA=0)

for(i in 1:168) {
  dic$Names[i] = doce_names[i]
  dic$R_0[i] = sum(doce_sub[i]==0 & !is.na(doce_sub[i]))
  dic$R_1[i] = sum(doce_sub[i]==1 & !is.na(doce_sub[i]))
  dic$R_2[i] = sum(doce_sub[i]==2 & !is.na(doce_sub[i]))
  dic$R_3[i] = sum(doce_sub[i]==3 & !is.na(doce_sub[i]))
  dic$R_4[i] = sum(doce_sub[i]==4 & !is.na(doce_sub[i]))
  dic$R_5[i] = sum(doce_sub[i]==5 & !is.na(doce_sub[i]))
  dic$R_6[i] = sum(doce_sub[i]==6 & !is.na(doce_sub[i]))
  dic$R_NA[i] = sum(is.na(doce_sub[i]))
  dic$Total = rowSums(dic[ , 4:11])
  }

# Se observa el resultado final.
dic

#============================================================================================================

# Verificamos que no existen errores en las variables de respuestas.

sum(dic[-c(seq(1:15), seq(24,29), 33, 41, 48, 60, 62, 69, 84, 106, 123, 168),]$Total!=2207)

#============================================================================================================

#~~~~~~~~~~~~~~~~~~~~~~~~#
# Docentes Seleccionados #
#~~~~~~~~~~~~~~~~~~~~~~~~#

# Repetimos el proceso con los datos de los docentes seleccionados

setwd("C:/Users/user/Desktop/Universidad/Muestreo/Docentes - TIC/Docentes")

RRHH <- read_excel("docentes_seleccionados.xls")
RRHH <- RRHH[, c(28, seq(1, 27))]
RRHH_names <- colnames(RRHH) # lo utilizaremos para el diccionario 

# Codificamos los nombres de las columnas.

for(i in 1:28) {
  colnames(RRHH)[i]= paste("do", i+168, sep = "")
}

# Unificamos el nombre para unir las bases mas adelante
colnames(RRHH)[1]= "do1"

# Elaboramos otro diccionario de los datos

dic_RRHH <- RRHH %>% 
  summary.default %>% as.data.frame %>% 
  dplyr::group_by(Var1) %>%  
  tidyr::spread(key = Var2, value = Freq) %>% 
  select(-Class, -Length)
  
for(i in 1:28) {
  dic_RRHH$Names[i] = RRHH_names[i]
}

dic_RRHH

#============================================================================================================

# Union de las bases de datos.

validation <- merge(x = doce_sub, y = RRHH, by ="do1")

#============================================================================================================

# Eliminar duplicados y NA de do1 (id)

sum(table(validation$do1)>1)
sum(is.na(validation$do1))

v_2 <- validation %>% group_by(do1) %>% slice(1) %>% filter(!is.na(do1))

sum(table(v_2$do1)>1)
sum(is.na(v_2$do1))

#============================================================================================================

#-------------------#
# Tasa de respuesta #
#-------------------#

# Poblacion que respondio la encuesta = doce_sub
# Poblacion muestral total = RRHH

nrow(doce_sub)/nrow(RRHH)*100

#============================================================================================================

# Creamos un diccionario para la validacion de datos

val_names <- c(doce_names, RRHH_names[-1])

dic_val <- v_2 %>% 
  summary.default %>% as.data.frame %>% 
  dplyr::group_by(Var1) %>%  
  tidyr::spread(key = Var2, value = Freq) %>% 
  select(-Class, -Length) %>% 
  mutate(Names = 0,
         R_0= 0,
         R_1= 0,
         R_2= 0,
         R_3= 0,
         R_4= 0,
         R_5= 0,
         R_6= 0,
         R_NA=0,
         Total=0)

for(i in 1:195) {
  dic_val$Names[i] = val_names[i]
  dic_val$R_0[i] = sum(v_2[i]==0 & !is.na(v_2[i]))
  dic_val$R_1[i] = sum(v_2[i]==1 & !is.na(v_2[i]))
  dic_val$R_2[i] = sum(v_2[i]==2 & !is.na(v_2[i]))
  dic_val$R_3[i] = sum(v_2[i]==3 & !is.na(v_2[i]))
  dic_val$R_4[i] = sum(v_2[i]==4 & !is.na(v_2[i]))
  dic_val$R_5[i] = sum(v_2[i]==5 & !is.na(v_2[i]))
  dic_val$R_6[i] = sum(v_2[i]==6 & !is.na(v_2[i]))
  dic_val$R_NA[i] = sum(is.na(v_2[i]))
  dic_val$Total = rowSums(dic_val[ , 4:11])
}

# Se saca el nombre debido a su longitud para ilustrar los datos.

select(dic_val, -Names)[17,]

#============================================================================================================

# Verificamos que no existen errores en las variables de respuestas.

sum(dic_val[-c(seq(1:15),seq(24,29),33,41,48,60,62,69,84,106,123,seq(168,195)),]$Total!=1965)

#============================================================================================================

#------------------#
# Datos Relevantes #
#------------------#

table(v_2$do178)
mean(v_2$do178=="Femenino")*100

# Predominancia del genero femenido del 71.25%

#============================================================================================================

#---------------------#
# Institución y Cargo #
#---------------------#

v_2 %>% select(do173) %>% 
  group_by(do173) %>% summarise(n=n()) %>% 
  filter(n>15) %>% ggplot(aes(n, do173)) + 
  geom_point() + scale_x_continuous(breaks = seq(10, 80, 10)) +
  ggtitle("Prevalencia de la Institución") +
  ylab("") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# El grafico nos ilustra la predominancia de los docentes por institucion educativa que se encuentran

v_2 %>% select(do186) %>% 
  group_by(do186) %>% summarise(n=n()) %>% 
  filter(n>25) %>% ggplot(aes(n, do186)) + 
  geom_point() + scale_x_continuous(breaks = seq(25, 425, 50)) +
  ggtitle("Prevalencia del cargo") +
  ylab("") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# 

#============================================================================================================

#-------------------#
# Edad y Antiguedad #
#-------------------#

v_2 %>% 
  ggplot(aes(as.numeric(do179), fill=do175)) + 
  geom_density(alpha=0.2)  +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  facet_grid(~do178) +
  xlab("Edad") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

v_2 %>% 
  ggplot(aes(as.numeric(do180), fill=do175)) + 
  geom_density(alpha=0.2)  +
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  facet_grid(~do178) +
  xlab("Antiguedad") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

#============================================================================================================

#-----------------#
# Uso de Internet #
#-----------------#

v_2 %>% filter(!is.na(do49)) %>%
  group_by(do49) %>%
  ggplot(aes(as.character(do49), fill=do175)) + 
  geom_bar(position="dodge") +
  facet_grid(~do178) +
  xlab("Uso del Internet") +
  ylab("") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# 1- No tengo acceso a internet
# 2- Menos de 1 hora
# 3- De 1 a 3 horas
# 4- De 4 a 6 horas
# 5- De 7 a 9 horas
# 6- Más de 9 horas

v_2 %>% filter(!is.na(do61)) %>%
  group_by(do61) %>%
  ggplot(aes(as.character(do61), fill=do175)) + 
  geom_bar(position="dodge") +
  facet_grid(~do178) +
  xlab("Uso del Internet") +
  ylab("") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# 1- En su casa
# 2- En la casa de un amigo
# 3- En la casa de un pariente
# 4- En la institución educativa
# 5- En algún lugar público (shopping, café, biblioteca, plaza, etc.)
# Otro

#============================================================================================================

#----------------------#
# Uso de la tecnología #
#----------------------#

# Generamos el recuento de equipos tecnológicos en el hogar

Tecno <- data.frame(N=as.numeric(lapply(17:23, function(i){
  sum(!is.na(v_2[,i]) & v_2[,i]==1)
})))

# Preparamos el primer dataset

Tecno <- Tecno %>% 
  mutate(Names=c("PC", "Notebook", "Tablet", "Celular", "TV", "Radio", "No tiene"), 
         Porcentaje = round((N/1965*100), digits = 2)) %>%
  select(Names, N, Porcentaje)

# Agregamos el uso frecuente de cada tecnología

Tecno <- Tecno %>% mutate(A_diario=c(as.numeric(t(data.frame(lapply(c(50,51,52,54,56,53), function(i){
  data.frame(table(v_2[,i])) %>% select(Freq)
})))[,1]), NA), 
Una_semanal=c(as.numeric(t(data.frame(lapply(c(50,51,52,54,56,53), function(i){
  data.frame(table(v_2[,i])) %>% select(Freq)
})))[,2]), NA), 
Rara_vez=c(as.numeric(t(data.frame(lapply(c(50,51,52,54,56,53), function(i){
  data.frame(table(v_2[,i])) %>% select(Freq)
})))[,3]), NA), 
Ninguna_vez=c(as.numeric(t(data.frame(lapply(c(50,51,52,54,56,53), function(i){
  data.frame(table(v_2[,i])) %>% select(Freq)
})))[,4]), NA))

# Observamos el resultado final
Tecno
rowSums(Tecno[1,4:6])

#============================================================================================================
