#~~~~~~~~~~~~~~#
# Proyecto CyT #
#~~~~~~~~~~~~~~#

library(readxl)
library(tidyverse)
library(hrbrthemes)
setwd("C:/Users/user/Desktop/Universidad/Ciencias y Tecnologia/Proyecto de Ciencia y Tecnologia/Proyecto Final/Dataset")
Data <- read_excel("Data.xlsx")

# Cargamos la base de datos

# La Moda es el número que más se repite dentro de un conjunto
# La Media es el valor promedio de ese conjunto de números
# La Mediana es aquel que se encuentra en el medio.

#============================================================================================================

#--------------------------#
# Preparamos las variables #
#--------------------------#

Data$Genero <- as.factor(ifelse(Data$Femenino==1, "F", "M"))
Data$Titulo <- as.factor(ifelse(Data$Grado==1, "G", 
                                ifelse(Data$`Especialidad / Diplomado`==1,"E", 
                                                           ifelse(Data$Maestría==1, "M", 
                                                                  ifelse(Data$Doctorado==1, "D", "PD")))))


# G = Grado
# E = Especialidad/Diplomado
# M = Master
# D = Doctorado
# PD = Post Doctorado

Data <- Data %>% select(-c("Post-Doctorado", "Doctorado", "Maestría", 
                   "Especialidad / Diplomado", "Grado", "Femenino", 
                   "Masculino"))

#============================================================================================================

#---------------------#
# Exploración Inicial #
#---------------------#

nrow(Data)
ncol(Data)

# Nuestro Dataset cuenta con 102 filas y 23 columnas.

table(Data$Genero)
mean(Data$Genero=="M" & Data$Titulo=="D")*100
mean(Data$Genero=="F" & Data$Titulo=="D")*100
Data %>% ggplot(aes(Genero, fill=Titulo)) + geom_bar() +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))


# Notamos que existe una predominancia del genero masculino del 81,4%.


table(Data$Titulo)
mean(Data$Titulo=="D")*100

# La predominancia del mayor nivel de estudios alcanzado es de la categoria de doctorados con un 79,4%.


#-------------------------------------------------------------------------------------------------------------
mean(Data$`Participó en proyectos financiados por CONACYT` & Data$`Ejerce la docencia?`|
       Data$`Participó en proyectos financiados por organismos internacionales` & Data$`Ejerce la docencia?`)*100

# Distribucion por nivel del Investigador


table(Data$Categoría)

# Distribucion por categoria del Investigador

#-------------------------------------------------------------------------------------------------------------
mean(Data$`Año de obtención del ultimo grado académico`)

# Promedio desde la obtencion del ultimo grado académico

mean(Data$`Años desde la Obtención del Ültimo grado académico`)

# Promedio de años desde la obtencion del ultimo grado académico

mean(Data$Edad)

# Promedio de la edad de los investigadores

table(Data$`Estudió en el extranjero?`, Data$Titulo, dnn = c("Extranjero", "Titulo"))

# Distribucion de investigadores que estudiaron en el extranjero y su titulo obtenido

#-------------------------------------------------------------------------------------------------------------
mean(Data$`Estudió en el extranjero?`)*100

# Porcentaje que estudio en el extranjero

mean(Data$`Fue becado?`)*100

# Porcentaje que fue becado

mean(Data$`Estudió en el extranjero?` & Data$`Fue becado?`)*100

# Porcentaje que estudio en el extranjero y fue becado

table(Data$`Estudió en el extranjero?`, Data$`Fue becado?`, dnn = c("Extranjero", "Becado"))

# Distribucion de investigadores que estudiaron en el extranjero y fueron becados

#-------------------------------------------------------------------------------------------------------------

mean(Data$`Trabaja en una Institución Pública?`)*100

# Porcentaje que Trabaja en una Institución Pública

mean(Data$`Se hace investigación en el lugar donde trabaja?`)*100

# Porcentaje donde Se hace investigación en el lugar donde trabaja

mean(Data$`Trabaja en una Institución Pública?` & Data$`Se hace investigación en el lugar donde trabaja?`)*100

# Porcentaje que Trabaja en una Institución Pública y Se hace investigación en el lugar donde trabaja
#-------------------------------------------------------------------------------------------------------------

mean(Data$`Ejerce la docencia?` & Data$`Se hace investigación en el lugar donde trabaja?`)*100

# Porcentaje que Ejerce la docencia

mean(Data$`En una institución pública?`)*100

# Porcentaje que Ejerce la docencia en una institución pública

#-------------------------------------------------------------------------------------------------------------

mean(Data$`Participó en proyectos financiados por CONACYT`)*100

# Porcentaje que Participó en proyectos financiados por CONACYT

mean(Data$`Participó en proyectos financiados por organismos internacionales`)*100

# Porcentaje que Participó en proyectos financiados por organismos internacionales

#============================================================================================================

#-------------------#
# Indice-h vs Citas #
#-------------------#

Data %>% ggplot(aes(Citas,`Indice-h`, color=Genero)) + geom_point() + geom_smooth(method = lm) +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# En este grafico podemos observar la distribucion del Indice-h vs el numero de citas 
# con sus respectivas tendencias clasificadas por sexo.
# Notamos una correlacion positiva y una pendiente mas elastica para los hombres. 
# Esto quiere decir que ante mayor numero de citas, mayor es tambien el indice-h. 

Data %>% ggplot(aes(Citas,`Indice-h`, color=Genero)) + geom_point() + geom_smooth() +
  ggtitle("Distribucion del Indice-h y el numero de Citas por genero")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# Este grafico nos señala una tendencia mas precisa de los datos 
# con el respectivo intervalo de confianza del 95%.

Data %>% ggplot(aes(Citas,`Indice-h`, color=Nivel)) + geom_point(aes(shape=Genero), size=3)  +
  ggtitle("Distribucion del Indice-h y el numero de Citas por nivel y genero")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# Con este grafico notamos la clasificacion por colores del nivel de los investigadores.
# Podemos notar que un investigador de nivel II tiene mayo numero de citas tanto como de indice-h.
# Ademas se diferencia en el grafico los sexos por formas.

Data %>% ggplot(aes(Citas,`Indice-h`, color=Genero)) + geom_point(size=3) + facet_grid(~Nivel) +
  ggtitle("Distribucion del Indice-h y el numero de Citas por nivel y genero")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# En el grafico, se hace mas evidente la distribucion de las variables por nivel de investigador.
# Notamos que el nivel I cuenta con menos cantidad de citas e indice-h mientras que el nivel II es lo contrario.


Data %>% ggplot(aes(`Indice-h`, fill=Nivel)) + geom_density(alpha=0.2)  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

Data %>% ggplot(aes(`Indice-h`, fill=Titulo)) + geom_density(alpha=0.2)  +
  ggtitle("Distribucion del Indice-h y el numero de Citas por nivel y genero")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

Data %>% ggplot(aes(`Indice-h`, fill=Nivel)) + geom_density(alpha=0.2)  +
  facet_grid(Titulo~Genero)+
  ggtitle("Distribucion del Indice-h y el numero de Citas por nivel y genero")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))


#============================================================================================================

#------------------#
# Indice-h vs Edad #
#------------------#

Data %>% ggplot(aes(Edad,`Indice-h`, color=Genero)) + geom_point() + facet_wrap(~Nivel) + 
  ggtitle("Distribución del Indice-h vs la Edad del Investigador")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# En el presente grafico tenemos la distribucion por edad vs el indice-h 
# dividido por el nivel del investigador y clasificado por sexo
# Notamos que para los candidatos a investigador, los valores se centran mas entre los 30 y 45 años de edad
# similar a los de nivel I.
# mientras que para los de nivel II, el rango esta entre los 40 y 55 años con un indice-h promedio superior.

Data %>% ggplot(aes(Edad,`Indice-h`, color=Nivel)) + geom_point(aes(shape=Genero), size=3) + 
  scale_x_continuous(breaks=seq(28, 100, 5)) +
  ggtitle("Distribución del Indice-h vs la Edad del Investigador")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# Este grafico nos presenta el indice-h vs la edad clasificado por el nivel y genero del investigador.
# notamos una predominancia de los investigadores entre los 28 y 48 años y un indice-h de maximo 15.

#============================================================================================================

#--------------------------#
# Indice-h vs Edad y Citas #
#--------------------------#

Data %>% ggplot(aes(Edad,`Indice-h`, color=Nivel)) + 
  geom_point(aes(size=Citas),alpha=1) + 
  geom_text(aes(label=Citas), nudge_x = 1.5)+ 
  scale_x_continuous(breaks=seq(28, 100, 5)) +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# El presente grafico nos muestra la clasificacion de los investigadores por nivel y numero de citas. 

#============================================================================================================

#----------#
# Estudios #
#----------#

Data %>% ggplot(aes(Titulo,`Indice-h`)) + 
  geom_boxplot() +
  labs(fill = c("M=Master","D=Doctorado","PD=Post Doctorado")) +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

Data %>% ggplot(aes(Nivel,`Indice-h`)) + 
  geom_boxplot() +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

Data %>% ggplot(aes(Nivel,`Indice-h`, color=Titulo)) + 
  geom_boxplot() +
  ggtitle("Distribución del Indice-h vs la Edad del Investigador")  +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

#============================================================================================================

#---------------------#
# Grado Universitario #
#---------------------#

Uni <- Data %>% select(`Universidad dónde obtuvo su último grado universitario`) %>% 
  group_by(`Universidad dónde obtuvo su último grado universitario`) %>% summarise(n=n())

Uni %>% filter(n>1) %>% ggplot(aes(n, `Universidad dónde obtuvo su último grado universitario`)) + 
  geom_point() + scale_x_continuous(breaks = seq(0, 8)) +
  ylab("") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# En el grafico podemos notar que algunos investigadores realizaron sus estudios en unviversidades similares.
# Se destaca la Universidad de Sevilla, España y la UNA

#============================================================================================================

#------------#
# Afiliación #
#------------#

Afi <- Data %>% select(`Institución de afiliacion`) %>% 
  group_by(`Institución de afiliacion`) %>% summarise(n=n())

Afi %>% filter(n>1) %>% ggplot(aes(n, `Institución de afiliacion`)) + 
  geom_point() + scale_x_continuous(breaks = seq(0, 11)) +
  ylab("") +
  theme_ipsum() + 
  theme(plot.title = element_text(size=15))

# En el grafico podemos notar que algunos investigadores estan afiliados a Instituciones similares.
# Se encuentra una principal predominancia de diferentes sectores de la UNA

#============================================================================================================
















































































































