#Created on Tue March  8 11:49:58 2021
#@author: mayukh

#importing libraries
library(tm)
library(wordcloud)
library(lubridate)
library(scales)
library(dplyr)
library(ROAuth)
library(NLP)
library(RColorBrewer)
library(syuzhet)
library(reshape2)
library(igraph)
library(rtweet)
library(ggmap)
library(igraph)
library(tidyverse)
library(ggraph)
library(ggplot2)
library(data.table)
library(maps)
library(mapdata)
library(mltools)

#Tweeter API
token <- rtweet::create_token(
  app = "SIN Project 2021",
  consumer_key <- "snzYDheo2HrohdHI903oqzAE1",
  consumer_secret <- "sisPofd4Hw117pI59C2cqnnqnABzITBkPTtRynaM2Fl9qNnYKi",
  access_token <- "1124360518439821313-QQtqIm93OVWek0nj9vGzR8Y2zA3iT8",
  access_secret <- "kR9MoTnVdg2Uf4t0YXjtoBuLk0M5MEJ3X29NzEIXqWUPW")

rtweet::get_token()

#Gather Recent Tweets
covidTweets <- rtweet::search_tweets("Coronavirus OR coronavirus OR COVID19 OR covid19, lang:en", n = 1000, type = "recent", retryonratelimit = FALSE)

# Build corpus
corpus <- iconv(covidTweets$text,from = "latin1", to = "ascii", sub = "")
corpus <- Corpus(VectorSource(corpus))

# Clean text
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset <- tm_map(cleanset, removeWords, c('corona','covid','positive','coronavirus'))
cleanset <- tm_map(cleanset, stripWhitespace)

#Sentiment of the tweets
s <- get_nrc_sentiment(cleanset[]$content)
s[s>1] <- 1
t(head(s))

barplot(colSums(s), las = 2, col = rainbow(50))

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10,1:10]
tdm <- tdm[rowSums(tdm)>30,]

# Word Frequency Histogram
w <- rowSums(tdm)
w <- subset(w, w>=25)
w <- sort(rowSums(tdm), decreasing = TRUE)
barplot(w, las = 2, col = rainbow(50))

#Word Cloud
wordcloud(words = names(w), freq = w, max.words = 1500, random.order = F, min.freq = 1, colors = brewer.pal(8, 'Dark2'), scale = c(1.5, 1.5), rot.per = 0.5)

############################################################# Network of terms #####################################################################
tdm[tdm>0] <- 1
termM <- tdm %*% t(tdm)
termM[1:10,1:10]
g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
g
g <- igraph::simplify(g)
is_simple(g)

V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

# Network diagram
plot(g)
tkplot(g,vertex.color='yellow')
plot(g,
     vertex.label.dist = 1.5,
     vertex.label = NA)

# Community detection
comm <- cluster_edge_betweenness(g)
plot(comm, g, vertex.label.dist = 1.5)
plot(comm, g, vertex.label.dist = 1.5, vertex.label = NA )


prop <- cluster_label_prop(g)
plot(prop, g, vertex.label.dist = 1.5 )
plot(prop, g, vertex.label.dist = 1.5, vertex.label = NA )

greed <- cluster_fast_greedy(g)
plot(greed, g, vertex.label.dist = 1.5 )
plot(greed, g, vertex.label.dist = 1.5, vertex.label = NA )

# Highlighting degrees
V(g)$label.cex <- V(g)$degree / max(V(g)$degree) + 0.3
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight) + .4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g,
     vertex.color='green',vertex.dist = 15,
     vertex.size = V(g)$degree*.5)

tkplot (g,vertex.color='green',vertex.dist = 15,
        vertex.size = V(g)$degree*.5)

#ANALYSIS OF TERMS 
grp <- greed # must be changed to the cluster we are analysing
for (x in unique(grp$membership)){
  y <- get_nrc_sentiment(subset(grp$names,grp$membership==x))
  meanValue <- colMeans(y)
  print(paste("Group ",x," : "))
  print(paste(" Number of memebers : ",nrow(y)))
  print(paste("RMSE : ",rmse(meanValue,y)))
}

x <- 2 #This is the cluster number, to plot its sentiment score
y <- get_nrc_sentiment(subset(grp$names,grp$membership==x))
meanValue <- colMeans(y)

barplot(meanValue, las = 2, col = rainbow(50))

############################################################# Network of tweets #####################################################################
 
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10,1:7]
tdm <- t(tdm)
tdm <- tdm[rowSums(tdm)>20,colSums(tdm)>15]

tdm[tdm>1] <- 1

tweetM <- tdm %*% t(tdm)

tweetM[1:10,1:10]



g <- graph.adjacency(tweetM, weighted = T, mode = 'undirected')
g
g <- igraph::simplify(g)
is_simple(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)


# Histogram of node degree
hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Degree',
     ylab = 'Freuqency',
     xlab = 'Degree')

# Network Diagram
plot(g)
plot(g,
     vertex.size = 6,
     vertex.label = NA)

# Community detection
comm <- cluster_edge_betweenness(g)
plot(comm,g)
plot(comm, g,vertex.size = 10,
     vertex.label.dist = 1.5,
     vertex.label = NA )

prop <- cluster_label_prop(g)
plot(prop, g, vertex.size = 10,
     vertex.label.dist = 1.5)
plot(prop, g, vertex.size = 10,
     vertex.label = NA )

greed <- cluster_fast_greedy(as.undirected(g))
plot(greed, g, vertex.size = 10,
     vertex.label.dist = 1.5)
plot(greed, g, vertex.size = 10,
     vertex.label.dist = 1.5,
     vertex.label = NA)

# Highlighting degrees
V(g)$label.cex <- V(g)$degree / max(V(g)$degree) + 0.3
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight) + .4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g,
     vertex.color='green',
     vertex.size = V(g)$degree*.5)

tkplot (g,vertex.color='green',vertex.dist = 15,
        vertex.size = V(g)$degree*.5)

#ANALYSIS OF THE TWEETS
grp <- greed # must be changed to the cluster we are analysing
for (x in unique(grp$membership)){
  y <- s[subset(grp$names,grp$membership==x),]
  meanValue <- colMeans(y)
  print(paste("Group ",x," : "))
  print(paste(" Number of memebers : ",nrow(y)))
  print(paste("RMSE : ",rmse(meanValue,y)))
  
}

x <- 6 #This is the cluster number, to plot its sentiment score
y <- s[subset(grp$names,grp$membership==x),]
meanValue <- colMeans(y)

barplot(meanValue, las = 2, col = rainbow(50))




















