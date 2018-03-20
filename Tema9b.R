
#
# ================= Preparaci??n previa =================
#

# CONFIGURAR el directorio de trabajo
setwd("/Desktop/datos")

# Cargar la librer??a igraph
library(igraph)

#
# ================= Creando grafos =================
#

# Ejemplo de multigrafo dirigido
g<-graph(c(1,2, 2,3, 3,2, 2,4, 1,4, 1,2), directed=T)
plot(g)

# Cargar el fichero de datos
g<-read.graph('carreteras.ncol', format='ncol', directed=F)

# Listar los v??rtices
V(g)

# Listar los arcos
E(g)

# Ver un arco en concreto
E(g)[[3]]

#
# ================= Manipulaci??n b??sica de grafos =================
#

# === Eliminar v??rtices y arcos ===
# Eliminar v??rtices
V(g)
g<-delete.vertices(g, c('Badajoz'))
V(g)

# Eliminar arcos
E(g)
g<-delete.edges(g, E(g, P=c('Coru??a','Vigo','Gerona','Barcelona')))
E(g)

# === A??adir v??rtices y arcos ===
# Empezamos cargando de nuevo el grafo inicial de fichero
g<-read.graph('Tema9b.carreteras.ncol', format='ncol', directed=F)

# A??adimos v??rtices ...
# El segundo par??metro es el n??mero de v??rtices a a??adir
V(g)
g<-add.vertices(g, 1, attr=list(name='C??ceres'))
V(g)

# A??adimos arcos ...
E(g)
g<-add.edges(g, c('C??ceres', 'Madrid', 'C??ceres', 'Badajoz'), attr=list(weight=c(250,95)))
E(g)

# === Acceder a los atributos ===
V(g)$name
E(g)$name
V(g)$weight
E(g)$weight
V(g)$color
E(g)$color

# === Manipular los atributos ===
# Que el nombre de los arcos coincida con el peso
E(g)$name
E(g)$name<-E(g)$weight
E(g)$name

# Colores en los arcos en funci??n de la distancia
E(g)$color
E(g)[E(g)$weight <= 100]$color <- "Green"
E(g)[E(g)$weight > 100 & E(g)$weight <= 300]$color <- "Yellow"
E(g)[E(g)$weight > 300]$color <- "Red"
E(g)$color


#
# ================= Representaci??n gr??fica de grafos ================= 
#

# Podemos pintarlos con la funci??n plot...
plot(g)

# Podemos configurar el aspecto de la figura
# A??adimos un atributo propio (forma) a los v??rtices, con valores 'circle' o 'square'
V(g)$forma<-rep('circle', length(V(g)))
V(g)[V(g)$name=='Madrid']$forma<-"square"
plot(g, 
	vertex.size=20, 
	vertex.label=toupper(substr(V(g)$name,1,3)), 
	vertex.shape=V(g)$forma, 
	edge.width=3, 
	edge.lty='dashed', 
	edge.label=E(g)$name)


# === Layouts ===

# Layout circle
plot(g, 
	vertex.size=20, 
	vertex.label=toupper(substr(V(g)$name,1,3)), 
	vertex.shape=V(g)$forma, 
	edge.width=3, 
	edge.lty='dashed', 
	edge.label=E(g)$name,
	layout=layout.circle(g)
	)


# Layout Fruchterman-Reingold
plot(g, 
	vertex.size=20, 
	vertex.label=toupper(substr(V(g)$name,1,3)), 
	vertex.shape=V(g)$forma, 
	edge.width=3, 
	edge.lty='dashed', 
	edge.label=E(g)$name,
	layout=layout.fruchterman.reingold(g)
	)


# Layout en estrella
plot(g, 
     vertex.size=20,
     vertex.label=toupper(substr(V(g)$name,1,3)), 
     vertex.shape=V(g)$forma, 
     edge.width=3, 
     edge.lty='dashed', 
     edge.label=E(g)$name,
     layout=layout.star(g)
)


# Ejemplo de uso de layout propio
ej<-graph(c(1,2, 2,3, 2,4), directed=T)
plot(ej)

m<-matrix(c(10,100,20,100,30,150,30,50), 4,2, byrow=T)
m
plot(ej, layout=m)


#
# ================= An??lisis b??sico del grafo =================
#

# === Par??metros b??sicos ===

length(V(g))
length(E(g))

# Conexo o no
is.connected(g)

# Simple o no
is.simple(g)

# Nombres de los vecinos de Zaragoza
V(g)[neighbors(g,'Zaragoza')]$name

# Distancia m??xima entre dos nodos cualesquiera de la red
# En km (usando weights)
diameter(g)
# En saltos (usando un weights propio)
diameter(g, weights=rep(1,length(E(g))))

# Densidad
graph.density(g)


# === Rutas ===

# No tiene en cuenta el weights, s??lo los saltos!
average.path.length(g)

# Informaci??n sobre caminos m??s cortos
paths<-get.shortest.paths(g, from='Vigo', to='Murcia')
# Como puede haber varios de igual coste (m??nimo) nos quedamos con el 1o
V(g)[as.vector(paths$vpath[[1]])]$name


#
# ================= Medidas de centralidad =================
#

closeness(g)
closeness(g, "Sevilla")

betweenness(g)
betweenness(g, 'Valladolid')

degree(g)
degree(g, 'Albacete')

# Tres nodos con mayor degree
sort(degree(g), decreasing=T)[1:3]
