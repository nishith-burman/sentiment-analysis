

library(tm)
library(e1071)
library(dplyr)
library(caret)
library(ggplot2)
library(tidyverse)
library(textclean)
library(RCurl)
library(tidytext)
library(tidyr)
library(magrittr)
library(RColorBrewer)
library(wordcloud)
library(caTools)



reviews_original <- read.csv(file.choose(), stringsAsFactors = F)
reviews <- reviews_original
#reviews <- reviews_original[c(2,6,11)]
str(reviews)

table(reviews$airline_sentiment)
# So there are 2363 positive and 9178 negative reviews in out dataset

#reviews$text <- as.character(reviews$text)
#reviews_clean <- reviews %>%  unnest_tokens(word, text)

summary(reviews$airline_sentiment)


barplot <- ggplot(reviews, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar(color="black") +
  facet_grid(. ~ airline) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.margin = unit(c(3,0,3,0), "cm"))
barplot + scale_fill_brewer(palette="Oranges")

#datacleaning and exploring

### postive

positive.reviews <- reviews %>% filter( airline_sentiment == "positive")
positive.text <- positive.reviews$text
#positive.text <- unnest_tokens(positive.text)
positive.text <-replace_emoji(positive.text)
positive.text <- replace_non_ascii(positive.text)

positive.corpus <- gsub("http.*","",positive.text)
positive.corpus <- Corpus(VectorSource(positive.corpus))

positive.corpus <- tm_map(positive.corpus, removePunctuation)
positive.corpus <- tm_map(positive.corpus, stripWhitespace)
positive.corpus <- tm_map(positive.corpus, content_transformer(tolower))
positive.corpus <- tm_map(positive.corpus, removeNumbers)
positive.corpus <- tm_map(positive.corpus, removeWords, stopwords("english"))
positive.corpus <- tm_map(positive.corpus, removeWords, c("virginamerica", "united", "flight","jetblue",
                                                          "usairways","americanair","southwestair","get",
                                                          "hour","can","will","amp",'cant',"one",
                                                          "thank","flighted"))
positive.corpus <-  tm_map(positive.corpus, stripWhitespace)
inspect(positive.corpus)

positiveTDM <- TermDocumentMatrix(positive.corpus)
positiveTDM

matrix.positive <- as.matrix(positiveTDM) #matric of tdm
sorting.positive <- sort(rowSums(matrix.positive),decreasing = T) #sorting
dataframe.positive <- data.frame(word=names(sorting.positive),freq=sorting.positive) #frequency of words used


Spectral <- brewer.pal(8,"Spectral")
DarkColor <- brewer.pal(8,"Dark2")
barplot(dataframe.positive[1:20,]$freq, las = 2, 
        names.arg = dataframe.positive[1:20,]$word,
        col =Spectral, main ="Most frequent words",
        ylab = "Word frequencies")

wordcloud(dataframe.positive$word,dataframe.positive$freq, scale=c(3,0.5), max.words=2000, random.order=FALSE, rot.per=0.4, 
          colors=DarkColor,use.r.layout=T)

#negative

negative.reviews <- reviews %>% filter( airline_sentiment == "negative")
negative.text <- negative.reviews$text
#negative.text <- unnest_tokens(negative.text)
negative.text <-replace_emoji(negative.text)
negative.text <- replace_non_ascii(negative.text)

negative.corpus <- gsub("http.*","",negative.text)
negative.corpus <- Corpus(VectorSource(negative.corpus))

negative.corpus <- tm_map(negative.corpus, removePunctuation)
negative.corpus <- tm_map(negative.corpus, stripWhitespace)
negative.corpus <- tm_map(negative.corpus, content_transformer(tolower))
negative.corpus <- tm_map(negative.corpus, removeNumbers)
negative.corpus <- tm_map(negative.corpus, removeWords, stopwords("english"))
negative.corpus <- tm_map(negative.corpus, removeWords, c("virginamerica", "united", "flight","jetblue",
                                                          "usairways","americanair","southwestair","get",
                                                          "hour","can","will","amp",'cant',"one",
                                                          "thank","flighted"))
negative.corpus <-  tm_map(negative.corpus, stripWhitespace)
inspect(negative.corpus)

negativeTDM <- TermDocumentMatrix(negative.corpus)
negativeTDM

matrix.negative <- as.matrix(negativeTDM) #matric of tdm
sorting.negative <- sort(rowSums(matrix.negative),decreasing = T) #sorting
dataframe.negative <- data.frame(word=names(sorting.negative),freq=sorting.negative) #frequency of words used


Set1 <- brewer.pal(8,"Set1")
Blues <- brewer.pal(6,"Blues")
barplot(dataframe.negative[1:20,]$freq, las = 2, 
        names.arg = dataframe.negative[1:20,]$word,
        col =Set1, main ="Most frequent words",
        ylab = "Word frequencies")

wordcloud(dataframe.negative$word,dataframe.negative$freq, scale=c(3,0.5), max.words=200, random.order=FALSE, rot.per=0.25, 
          colors=Blues,use.r.layout=T)
