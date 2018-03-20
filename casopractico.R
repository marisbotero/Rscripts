#
#================================
#
# Caso práctico: Análisis de datos con R
#
#================================
#
datos<-read.table("coches.dat")

names(datos)
dim(datos)
head(datos)
datos[10:15,]
tail(datos)
which(datos=='?', arr.ind=T)
aborrar<-which(datos=='?', arr.ind=T)[,1]
datosn1<-datos[-aborrar,]
dim(datos)
marcaA<-datos$V1
marcaB<-datos$V2
