#text mining

rm(list=ls())

library(tm) # Framework for text mining.

library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) 
library(readr)
library(wordcloud)
library(Rgraphviz)

#experimental package
#source('http://bioconductor.org/biocLite.R')
#biocLite('Rgraphviz')


##load
x <- read_csv("C:/Users/Eric/Dropbox/AAARL_Shared/Workshop/Workshop Curriculum/Part III_ Tools in Machine Learning/Data for Part III/Tweets.csv")

#one week of tweets from sept 28 to oct 2 in 2017

corp1 <- Corpus(DataframeSource(x))
corp1 <- tm_map(corp1, content_transformer(tolower))
corp1 <- tm_map(corp1, removeWords, c("toronto", "housing", "the")) 

#Put into doc matrix
dtm <- DocumentTermMatrix(corp1)
findFreqTerms(dtm, 25)
findFreqTerms(dtm, 50)

#plotting word freq
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)


wf <- data.frame(word=names(freq), freq=freq)
subset(wf, freq>50) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))




#word cloud
set.seed(123)
par(mar = rep(2, 4))
wordcloud(names(freq), freq, min.freq=25, colors=brewer.pal(6, "Dark2"), scale=c(4, 0.9))

plot(dtm, terms=findFreqTerms(dtm, lowfreq=25)[1:15], corThreshold=0.2)

#Try without the square brackets


