library(tm)

#
# Carga de documentos y acceso b??sico
#
docs<-c("Un documento", "Otro documento")
c1<-Corpus(VectorSource(docs))
c1

c1[[2]]
content(c1[[2]])

source<-URISource("http://www.gutenberg.org/cache/epub/2000/pg2000.txt", encoding="UTF-8")
hamlet<-Corpus(source)
hamlet

content(hamlet[[1]])
content(hamlet[[1]])[1:100]

#
# Operaciones de procesamiento previo
#
docs<-c("Este documento fue escrito en 2014. Autores: varios escritores")
corpus<-Corpus(VectorSource(docs))
content(corpus[[1]])

corpus<-tm_map(corpus, content_transformer(tolower))
content(corpus[[1]])

corpus<-tm_map(corpus, removePunctuation)
content(corpus[[1]])

corpus<-tm_map(corpus, removeNumbers)
content(corpus[[1]])

corpus<-tm_map(corpus, removeWords, stopwords("spanish"))
content(corpus[[1]])

corpus<-tm_map(corpus, stemDocument, "spanish")
content(corpus[[1]])

#
# TDM binaria
#
docs<-c("test document", "example to test", "test document and example document")
corpus<-Corpus(VectorSource(docs))
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightBin))
inspect(tdm)

#
# TDM TF
#
docs<-c("test document", "example to test", "test document and example document")
corpus<-Corpus(VectorSource(docs))
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
inspect(tdm)

#
# TDM TFxIDF
#
docs<-c("test document", "example to test", "test document and example document")
corpus<-Corpus(VectorSource(docs))
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTfIdf))
inspect(tdm)

#
# Varios ejemplos de an??lisis
#
data(acq)
data(crude)
corpus <- c(acq,crude)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
tdm <- TermDocumentMatrix(corpus)
inspect(tdm)

#
# K-palabras m??s frecuentes
#
findFreqTerms(tdm, 25)
findAssocs(tdm,'crude',0.65)

#
# Worcloud
#
# install.packages("wordcloud", dependencies=TRUE)
library(wordcloud)
tf<-rowSums(as.matrix(tdm))
tfordered<-sort(tf, decreasing=TRUE)
top15<-tfordered[1:15]
colors<-gray((top15+1)/(max(top15)+1))
wordcloud(names(top15), top15, colors=colors)
wordcloud(names(top15), top15, colors=brewer.pal(6,'Dark2'))

#
# Funci??n dissimilarity
# Similitud entre textos mediante distancia coseno entre vectores
#
dissimilarity <- function(x, y, method="cosine") {  
  corp <- VCorpus(VectorSource(c(x)))
  corp <- c(corp, y)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, stemDocument)
  corp <- tm_map(corp, PlainTextDocument)
  tdm <- TermDocumentMatrix(corp)
  proxy::dist(as.matrix(t(tdm)), method = method)
}

masSimilar <- function(x, y){
  h <- dissimilarity(x,y) 
  return(which.min(h[1:length(y)]))
}

library(tm)
data(crude)
data(acq)
corpus <- c(acq, crude)
masSimilar("crude price", corpus)

