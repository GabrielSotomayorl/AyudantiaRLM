#Instalar paquetes
install.packages("stargazer")
library(stargazer)
install.packages("texreg")
library(texreg)
install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)
install.packages("corrplot")
library("corrplot")
install.packages("faraway")
library(faraway)
install.packages("coefplot")
library(coefplot)

#Preparación de los datos----------------------------------------------

#Abrimos la base de datos
base<-read.csv("https://raw.githubusercontent.com/GabrielSotomayorl/AyudantiaRLM/master/baseayudantia.csv")
base<-base[-1] #se elimina el folio de los casos, ya que no se utilizará


#La base integra variables de percepciones del delito, creada a partir de la encuesta UDP 2015 
#contiene 5 variables
# 1) Sexo
# 2) P.politica, escala de 1 a 10, siendo 1 izquierda y 10 derecha
# 3) Detencion.ciudadana, Índice de 3 a 15 que mide el nivel de legitimación de las detenciones ciudadanas
# 4) Medidas.Punitivas, Índice de 3 a 15 que mide el nivel la percpeción de efectividad 
# las medias punitivas contra el delito
# 5) Gobierno.delincuencia, nota al gobierno en dismiución de la deincuencia de 1 a 7

#Se construira un modelo usando la legitimación de las detenciones ciudadanas como VD, y el resto como VD

#Revisar que tipo de variables son
str(base)
#Son todas integer (números enteros)

#Eliminar casos perdidos - na.omit es listwise deletion 
base<-na.omit(base) 

#Convertimos Sexo en factor para intorducirla en el modelo como varaible ficticia
base$Sexo=factor(base$Sexo,levels=(c(0, 1)), labels = (c("Hombre","Mujer")))

#Resumen de la Base de datos
install.packages("stargazer")
library(stargazer)
stargazer(base, type = "text", digits=1)

#Análisis previos a la ejecución del modelo
# 1) Relación lineal entre las varaibles y ausencia de multicolinealdiad entre los predictores

#Scatterplot para observar la correlación entre Detencion.ciudadana y Medidas.Punitivas
#Para hacer este gráfico usreamos el paquete ggplot2, con el cual podemos ir construyendo gráficos 
#por capas, habiendo dos elementos base aes, donde debemos poner los datos, y luego distintos
#geometrics (geom) donde asignamos como estos datos se traducen en formas y colores, en este caso
#pedimos puntos (color steelblue y el alpha muestra la transparencia que asegnamos a los puntos) 
#y una línea resúmen a partir de una función lineal (method="lm"), finalmente asignamos etiquetas

install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)

g=ggplot(base, aes(x=Detencion.ciudadana, y=Medidas.Punitivas)) +
  geom_point(color="steelblue",alpha=0.25) + geom_smooth(method="lm",col="red",se=F)+
  labs(x="Legitimación de detenciones ciudadanas",y="Efectividad de las medias punitivas")

gg=ggplotly(g)
gg 

#Corrplot
#Este paquete nos permite graficar rapidamente una matriz de correlaciones, traduciendo la fuerza de 
#la realción en tamaño y opacidad de los criculos y la dirección de la relación en color, azul-positiva
#y rojo negativo
install.packages("corrplot")
library("corrplot")
#Primero debemos guardar como objeto una matriz de correlaciones
cor<-cor(base[-3]) #Con -3 omitimos la variable sexo de la matriz de correlaciones
corrplot(cor)
#Este comando nos entrega un gráfico ligeramente diferente que pone a un lado los circulos y al otro los
#coeficietnes de correlación
corrplot.mixed(cor)
#A partir de esta matriz podemos observar que exista ausencia de multicolinealidad entre 
#las variables independientes, requisito para el calculo del modelo. En este caso todas nuestras 
#correlaciones tienen valores muy bajos, menores a 0,2, lo cual da cuenta de ausencia de correlación

#Estimación y visualización del modelo--------------------------------

#Estimar modelos de  RLM -- Controlar por cada variable, tener en cuenta la matriz de correlación para observar como varían los coeficientes

reg1=lm(Detencion.ciudadana ~ Sexo, data=base)  #Lectura de variables dicotómicas
reg2=lm(Detencion.ciudadana ~ Medidas.Punitivas + Sexo, data=base)
reg3=lm(Detencion.ciudadana ~ Gobierno.delincuencia + Medidas.Punitivas + Sexo, data=base)
reg4=lm(Detencion.ciudadana ~ P.politica + Gobierno.delincuencia + Medidas.Punitivas + Sexo, data=base)


#Visualización de distintos modelos con screenreg
install.packages("texreg")
library(texreg)
screenreg(list(reg1,reg2,reg3,reg4))

#Visualización de distintos modelos con stargazer
install.packages("stargazer")
library(stargazer)
stargazer(reg1,reg2,reg3,reg4, type = "text")

##Interpretación de los coeficientes
screenreg(reg4)
#Respecto de la interpretación de los beta podemos señalar que:
#1)	Por cada punto adicional en el índice de percepción de efectividad de las medidas punitivas 
#contra la delincuencia, el valor esperado de legitimidad de las DC aumenta en 0,51 puntos, controlando 
#por sexo, posición política y evaluación del gobierno en disminución de la delincuencia.
#2)	Las mujeres legitiman 0,47 puntos menos que los hombres las detenciones ciudadanas, controlando 
#por percepción de efectividad de la medidas punitivas, posición política y evaluación del gobierno 
#en disminución de la delincuencia.
#3)	Por cada punto adicional en la escala de posición política (es decir, una posición más a la derecha) 
#el valor esperado de legitimidad de la DC aumenta en 0,17 puntos, controlando por sexo, percepción de 
#efectividad de la medidas punitivas y evaluación del gobierno en disminución de la delincuencia.
#4)	Por cada punto adicional de evaluación de la capacidad del gobierno de disminuir
# la delincuencia, disminuye en 0,15 el valor esperado de la legitimación de las DC, 
#controlando por sexo, percepción de efectividad de la medidas punitivas y posición política.

#Evaluación de intervalos de confianza de los coeficientes beta

#Coefplot-

install.packages("coefplot")
library(coefplot)
coefplot(reg4)

coefplot(reg4)+
  scale_y_discrete(name=" ", limits=c("(Intercept)","Sexo","Medidas.Punitivas","Gobierno.delincuencia",
                                      "P.politica"),
                   labels=c("Intercepto","Sexo","Efectividad medidas punitivas","Nota al Gobierno:Delincuencia"
                            ,"Posición Política")) +
  scale_x_continuous(name = " ") +
  ggtitle("Gráficos de Coeficientes Beta")+
  theme_bw()


#Table output for Word and RMarkdown documents---
install.packages("huxtable")
library(huxtable)
install.packages("jtools")
library(jtools)
install.packages("broom")
library(broom)
export_summs(reg1,reg2,reg3,reg4, scale = TRUE)
