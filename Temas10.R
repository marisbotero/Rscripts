#
# Configuraci??n inicial 
setwd("/Users/cruiz/Desktop/datos")

#
# Cargamos los datos desde el fichero
#
datos<-read.table("Tema6.salud.csv", sep=" ", header=T)
head(datos)

#
# Hacemos que las columnas est??n disponibles como variables 
# independientes
#
attach(datos)
head(edad)

#
# Base graphics
#

# ========= Gr??ficos b??sicos ========= 

# Histograma (hist)
hist(datos$tension)
hist(tension)

# Especificando los l??mites de los intervalos
# La tensi??n va de 90 a 200
summary(tension) 
hist(tension, breaks=seq(90, 200, 5))

# Diagrama de caja (boxplot)
# De una sola variable
boxplot(colesterol)

# De la edad respecto al sexo (si el muestreo bien hecho 
# no deber??a haber mucha diferencia)
boxplot(edad ~ sexo)

# Diagrama de barras (barplot)
# Diagrama de barras por edades
# Vemos cu??les son la edad m??nima y m??xima
summary(edad)
cut(edad, seq(20, 80, 10))
table(cut(edad, seq(20, 80, 10)))
barplot(table(cut(edad, seq(20, 80, 10))))

# Tambien se pueden dibujar las barras en horizontal
barplot(table(cut(edad, seq(20, 80, 10))), horiz=T, las=1)

# Se pueden tabular dependencias de dos variables y representar...
# Ej: edad como factor y sexo como factor
table(cut(edad, seq(20, 80, 10)), sexo)

# Por defecto, representaci??n apilada (stacked)
barplot(table(cut(edad, seq(20, 80, 10)), sexo))

# Pero se puede hacer que las barras aparezcan unas al lado de otras
barplot(table(cut(edad, seq(20, 80, 10)), sexo), beside=T, ylim=c(0,120))

# Gr??fico circular (pie)
# Uso similar a barplot (de barras simples), menos recomendado
pie(table(cut(edad, seq(20, 80, 10))))

# Gr??fico de dispersi??n (plot)
plot(colesterol)
plot(edad, colesterol)

# ========= Personalizar gr??ficas =========
x<-seq(-5,5,0.2)
y<-(x**2)-x+1

plot(x,y)

# Dibujamos una l??nea
plot(x,y,type='l')

# Colores, tipos de l??nea, ejes, t??tulo, ...
plot(x, y,
	type = 'b', 
	pch = 0,
	cex = 0.5, 
	lty = 'dotted',
	lwd = 2,
	col = '#663399',
	main = expression(paste('Dibujando el polinomio y=',(x**2)-x+1)),
	xlab = "Eje X",
	ylab = "Eje Y",
	col.main = "blue"
	)
 
# Leyendas, rejillas y texto en puntos concretos ...
plot(x, y,
	type = 'b', 
	lty = 'dotted',
	col = '#663399',
	xlim = c(-2,2),
	ylim = c(0, 10),
	main = expression(paste('Dibujando el polinomio y=',(x**2)-x+1)), 	
	xlab = "Eje X",
	ylab = "Eje Y"
)

legend(x=-0.4, y=10, 
	pch = 1,
	title="Leyenda", 
	lty='dotted', 
	col='#663399', 
	legend=expression(paste('y=',(x**2)-x+1))
	)

# Calculamos y representamos el m??nimo de la funci??n en [0,1]
optimize(function(x) {(x**2)-x+1}, c(0,1))
text(x=0.5, y=1.3, labels=c("Minimo"), col='blue')

grid(col='red', lty='dashed', lwd='0.5')

# ========= Combinando gr??ficos =========

# En una sola figura con abline
plot(x, y,
	type = 'b', 
	lty = 'dotted',
	col = '#663399',
	xlim = c(-2,2),
	ylim = c(-10, 10)
)

abline(h=0, lty='dotdash', lwd=0.5)
abline(v=0, lty='dotdash', lwd=0.5)
abline(a=0, b=1, lty='dashed', col='red')

# En una sola figura con lines
plot(x, y,
	type = 'b', 
	lty = 'dotted',
	col = '#663399',
	xlim = c(-2,2),
	ylim = c(-10, 10)
)

lines(x, x**3, 
	type = 'b',
	lty = 'dashed',
	col = 'blue',  
	pch = 2)

grid(col='red', lty='dashed', lwd='0.5')

legend(x=-0.4, y=10, 
  title="Leyenda", 
	pch = c(1,2),	
	lty=c('dotted','dashed'), 
	col=c('#663399','blue'), 
	legend=c(expression(paste('y=',(x**2)-x+1)), expression(paste('y=',(x**3))))
	)

# En varias figuras ...
par(mfrow=c(2,1))
hist(colesterol)
hist(tension)

# ========= Exportar gr??fica a fichero =========
pdf('grafico.pdf')
par(mfrow=c(2,1))
hist(colesterol)
hist(tension)
dev.off()

#
# ggplot2 
#

# Simple histograma
library(ggplot2)
ggplot(datos, aes(x = colesterol)) +
  geom_histogram()

# Diagrama de cajas
ggplot(data=datos, aes(x=datos$diagnostico, y=datos$edad, fill=datos$diagnostico)) + 
  ylab("Edad") + xlab("Diagnostico") + 
  guides(fill=FALSE) +
  geom_boxplot()

# Diagrama de datos apilado
ggplot(datos,  aes(x = datos$diabetes, fill = datos$sexo) ) +
  xlab("Diagnosticados con diabetes") + ylab("Numero de pacientes") + 
  ggtitle("Distribucion sexo por pacientes con diabetes") +
  scale_fill_discrete(name="Sexo") +
  geom_bar()

# Ejemplo de selecci??n de datos, l??nea, puntos
e_factor <- cut(datos$edad, breaks=c(20,30,40,50,60,70,80), labels=c(30,40,50,60,70,80))
tab1 <- prop.table(table(e_factor, diagnostico), 1)*100
df.tab1 <- data.frame(tab1)
df.tab2 <- subset(df.tab1, diagnostico=="Pos")
ggplot(df.tab2, aes(x=e_factor, y=Freq, group=1)) + 
  ylim(0, 100) +
  geom_line() + 
  geom_point() +
  xlab("Grupos de edad") + 
  ylab("% de casos") + 
  ggtitle("Evolucion edad por casos positivos")

# Ejemplo de Regresi??n Lineal, Sesion 2
pred<-predict(lm(datos$colesterol ~ datos$edad, data=datos))
p1 <- ggplot(datos, aes(x = edad, y = colesterol))
p1 + geom_point(aes(color = colesterol)) + 
  geom_line(aes(y = pred)) + 
  geom_smooth(method=lm)


