#
#================================
#
# CLASE TEMA 3
#
#================================
#

# Establezca el path al lugar donde tenga los datos
#setwd("Desktop/datos") 

# Cargamos los datos desde el fichero
datos<-read.table(“Tema8.salud.data", sep=",")

# Tambi??n se podr??an leer directamente desde la URL
datos<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", sep=",")

# Exploramos los datos
names(datos)
dim(datos)
head(datos)
datos[10:15,]

# Exploramos la parte final de los datos (veremos que aparece ?)
tail(datos)

#================================

# -- Opci??n 1: eliminar las filas con ? --
# Encontrarlas ...
which(datos=='?', arr.ind=T)

# Eliminarlas ...
aborrar<-which(datos=='?', arr.ind=T)[,1]
datosn1<-datos[-aborrar,]

# Comprobar que se han borrado
dim(datos)
dim(datosn1)
tail(datosn1)


# -- Opci??n 2: al cargar los valores indicarle al sistema que los trate como NA ...
datos<-read.table("salud.data", sep=",", na.strings=c("?"))
# ... y despu??s omitimos los valores NA
datosn1<-na.omit(datos)


# IMPORTANTE: En realidad los valores NA s??lo aparecen en las columnas V12 y V13 que
# no vamos a usar en el an??lisis, as?? que, a partir de ahora trabajar?? con datos
# no con datosn1, de hecho lo m??s recomendable ser??a seleccionar primero los datos 
# con los que vamos a trabajar y luego limpiarlos


# Podemos hacer que la columna de sexo sea un factor (m??s legible)
datos$V2<-factor(datos$V2, levels=c(0,1), labels=c('Mujer', 'Hombre'))

# Y lo mismo con la columna de diabetes (con valores s?? o no)
datos$V6<-factor(datos$V6, levels=c(0,1), labels=c('No', 'Si'))

# Agrupar los niveles de la columna de enfermedad (enfermo si valor > 0)
datos$V14[datos$V14>0]<-1

# Convertir en factor la columna de enfermedad
# (con valores pos -enfermo- o neg -no enfermo-)
datos$V14<-factor(datos$V14, levels=c(0,1), labels=c('Neg', 'Pos'))


# Nos quedamos con un data frame m??s peque??o con las columnas de inter??s
edad<-datos$V1
sexo<-datos$V2
tension<-datos$V4
colesterol<-datos$V5
diabetes<-datos$V6
diagnostico<-datos$V14
d<-data.frame(edad,sexo,tension,colesterol,diabetes,diagnostico)

# Y con unos nombres de columnas m??s legibles
head(d)

#================================


# Una vez tenemos los datos, comenzamos el an??lisis
# 1) Estad??sticas del dataset
mean(d$edad)
median(d$edad)
sd(d$colesterol)
summary(d)

# 2.1) Dibujamos el histograma de edades
hist(d$edad, breaks=10)

# 2.2) Dibujamos el diagrama de caja de las edades
boxplot(d$edad)

# 3) Diagrama de caja de la distribuci??n de colesterol por sexo
boxplot(colesterol ~ sexo, data=d)


#================================


# 5) Chi square
# Sexo y diabetes son o no variables independientes?
# Hip??tesis nula: son independientes
# Creamos la tabla de contingencia
tabla<-table(d$sexo, d$diabetes)
tabla
chisq.test(tabla)

# Sexo y enfermedad cardiovascular son independientes?
# Hip??tesis nula: son independientes
tabla<-table(d$sexo, d$diagnostico)
tabla
chisq.test(tabla)

# 6) Las medias de colesterol de hombres y mujeres son las mismas
# Hip??tesis nula: s?? lo son (la diferencia de medias es 0)
t.test(d[d$sexo=="Hombre",]$colesterol, d[d$sexo=="Mujer",]$colesterol)


#================================


# 7) Existe correlaci??n entre edad y colesterol
cor.test(d$edad, d$colesterol)


# 8) Modelo lineal de colesterol vs edad
modelo<-lm(colesterol ~ edad, data=d)
summary(modelo)


#================================


# 9) Clasificador NaiveBayes para detectar si enfermo o no
# Entrenamos con una parte de los datos
library(e1071)
train<-d[1:250,]
clasificador<-naiveBayes(diagnostico ~ colesterol + tension + edad + diabetes, data=train)

# Y evaluamos con otra
eval<-d[251:303,]
diagnostico<-eval[,6]
predicted<-predict(clasificador, eval[,-6])
matrizconf<-table(predicted, diagnostico)
matrizconf
sum(diag(matrizconf))/sum(matrizconf)


