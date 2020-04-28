library(tm)
install.packages("RTextTools")
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
#library(doMC)
#registerDoMC(cores=detectCores())  # Use all available cores

reviews_original <- read.csv(file.choose(), stringsAsFactors = F )
reviews <- reviews_original[c(2,11)]
colnames(reviews) <- c("sentiment", "tweet")
table(reviews)
table(reviews$sentiment)
glimpse(reviews)
set.seed(1)
reviews<-reviews[sample(nrow(reviews)),]
reviews<-reviews[sample(nrow(reviews)),]
glimpse(reviews)
reviews$sentiment <- as.factor(reviews$sentiment)

reviews_text <- reviews$tweet
reviews_text <- replace_emoji(reviews_text)
reviews_text <- replace_non_ascii(reviews_text)

corpus <- Corpus(VectorSource(reviews_text))
inspect(corpus[1:3])


corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
dtm <- DocumentTermMatrix(corpus_clean)
inspect(dtm[40:50, 10:15])


reviews.train <- reviews[1:9232,]
reviews.test <- reviews[9233:11541,]

dtm.train <- dtm[1:9232,]
dtm.test <- dtm[9233:11541,]


corpus.clean.train <- corpus_clean[1:9232]
corpus.clean.test <- corpus_clean[9233:11541]


dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train,
                                   control=list(dictionary = fivefreq))
dim(dtm.train.nb)
dtm.test.nb <- DocumentTermMatrix(corpus.clean.test,
                                  control=list(dictionary = fivefreq))
dim(dtm.test.nb)
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)

classifier <- naiveBayes(trainNB, reviews.train$sentiment, laplace = 1)
pred <- predict(classifier, newdata=testNB)

table("Predictions"= pred,  "Actual" = reviews.test$sentiment )                 
conf.mat <- confusionMatrix(pred, reviews.test$sentiment)
conf.mat
conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']
