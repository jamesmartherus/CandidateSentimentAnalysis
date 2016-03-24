library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)
library(SnowballC)

setwd("/Users/jamesmartherus/Documents/TwitterScrapingProject/")

data <- read.csv("Sentiment_Analysis.csv")

###########
#Wordclouds
###########
data$tweet <- as.character(data$tweet)

#TRUMP
trump_corpus <- Corpus(VectorSource(data$tweet[data$candidate=="Trump"]))
trump_corpus <- tm_map(trump_corpus,
                       content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                       mc.cores=1)
trump_corpus <- tm_map(trump_corpus, content_transformer(tolower), lazy=TRUE)
trump_corpus <- tm_map(trump_corpus, PlainTextDocument, lazy=TRUE)
trump_corpus <- tm_map(trump_corpus, removePunctuation, lazy=TRUE)
#stopwords are things like I, me, etc.
trump_corpus <- tm_map(trump_corpus, removeWords, c(stopwords('SMART'),'http\\w+','[a-z][0-9]+.'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
trump_corpus <- tm_map(trump_corpus, stemDocument, lazy=TRUE)
wordcloud(trump_corpus, rot.per=.25,scale=c(5,.5), max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))

png("TrumpWordCloud",width=6,height=6, units="in", res=300)
wordcloud(trump_corpus,rot.per=.25, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))
dev.off()

#CRUZ
cruz_corpus <- Corpus(VectorSource(data$tweet[data$candidate=="Cruz"]))
cruz_corpus <- tm_map(cruz_corpus,
                       content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                       mc.cores=1)
cruz_corpus <- tm_map(cruz_corpus, content_transformer(tolower), lazy=TRUE)
cruz_corpus <- tm_map(cruz_corpus, PlainTextDocument, lazy=TRUE)
cruz_corpus <- tm_map(cruz_corpus, removePunctuation, lazy=TRUE)
#stopwords are things like I, me, etc.
cruz_corpus <- tm_map(cruz_corpus, removeWords, c(stopwords('SMART'),'http\\w+','[a-z][0-9]+.'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
cruz_corpus <- tm_map(cruz_corpus, stemDocument, lazy=TRUE)
wordcloud(cruz_corpus, rot.per=.25,scale=c(5,.5), max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))

png("CruzWordCloud",width=6,height=6, units="in", res=300)
wordcloud(cruz_corpus,rot.per=.25, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))
dev.off()


#KASICH
kasich_corpus <- Corpus(VectorSource(data$tweet[data$candidate=="Kasich"]))
kasich_corpus <- tm_map(kasich_corpus,
                      content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                      mc.cores=1)
kasich_corpus <- tm_map(kasich_corpus, content_transformer(tolower), lazy=TRUE)
kasich_corpus <- tm_map(kasich_corpus, PlainTextDocument, lazy=TRUE)
kasich_corpus <- tm_map(kasich_corpus, removePunctuation, lazy=TRUE)
#stopwords are things like I, me, etc.
kasich_corpus <- tm_map(kasich_corpus, removeWords, c(stopwords('SMART'),'http\\w+','[a-z][0-9]+.'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
kasich_corpus <- tm_map(kasich_corpus, stemDocument, lazy=TRUE)
wordcloud(kasich_corpus, rot.per=.25,scale=c(5,.5), max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))

png("KasichWordCloud",width=6,height=6, units="in", res=300)
wordcloud(kasich_corpus,rot.per=.25, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))
dev.off()


#CLINTON
clinton_corpus <- Corpus(VectorSource(data$tweet[data$candidate=="Clinton"]))
clinton_corpus <- tm_map(clinton_corpus,
                        content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                        mc.cores=1)
clinton_corpus <- tm_map(clinton_corpus, content_transformer(tolower), lazy=TRUE)
clinton_corpus <- tm_map(clinton_corpus, PlainTextDocument, lazy=TRUE)
clinton_corpus <- tm_map(clinton_corpus, removePunctuation, lazy=TRUE)
#stopwords are things like I, me, etc.
clinton_corpus <- tm_map(clinton_corpus, removeWords, c(stopwords('SMART'),'http\\w+','[a-z][0-9]+.'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
clinton_corpus <- tm_map(clinton_corpus, stemDocument, lazy=TRUE)
wordcloud(clinton_corpus, rot.per=.25,scale=c(5,.5), max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))

png("ClintonWordCloud",width=6,height=6, units="in", res=300)
wordcloud(clinton_corpus,rot.per=.25, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))
dev.off()

#SANDERS
sanders_corpus <- Corpus(VectorSource(data$tweet[data$candidate=="Sanders"]))
sanders_corpus <- tm_map(sanders_corpus,
                         content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                         mc.cores=1)
sanders_corpus <- tm_map(sanders_corpus, content_transformer(tolower), lazy=TRUE)
sanders_corpus <- tm_map(sanders_corpus, PlainTextDocument, lazy=TRUE)
sanders_corpus <- tm_map(sanders_corpus, removePunctuation, lazy=TRUE)
#stopwords are things like I, me, etc.
sanders_corpus <- tm_map(sanders_corpus, removeWords, c(stopwords('SMART'),'http\\w+','[a-z][0-9]+.'), lazy=TRUE)
#this converts all words to stem, i.e., standing to stand
sanders_corpus <- tm_map(sanders_corpus, stemDocument, lazy=TRUE)
wordcloud(sanders_corpus, rot.per=.25,scale=c(5,.5), max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))

png("SandersWordCloud",width=6,height=6, units="in", res=300)
wordcloud(sanders_corpus,rot.per=.25, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2"))
dev.off()

