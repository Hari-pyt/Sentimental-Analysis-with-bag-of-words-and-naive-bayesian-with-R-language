

## libraries

library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)


## we have a vector of stem negative words

## reading the file
setwd("C:/Users/Hari/Desktop/TermPaper/TermPaper")

movie<-read.csv("movie.csv",header = T,stringsAsFactors = F)

## we can take feedback directly from internet too
# read_html

summary(movie)



## randomizing the data set
set.seed(1)

movie<-movie[sample(nrow(movie)),]

movie<-movie[sample(nrow(movie)),]



movie$label=as.factor(movie$label)



moviec<-Corpus(VectorSource(movie$review))  #converting corpus


moviec

inspect(moviec[1:3])


length(moviec)




######cleaning the DATA
doc<-tm_map(moviec,removeNumbers)
doc<-tm_map(doc,removeWords,stopwords("english"))
doc<-tm_map(doc,removePunctuation)
doc<-tm_map(doc,stripWhitespace)
doc<-tm_map(doc,stemDocument,language="english")
doc<-tm_map(doc,tolower)
doc<-tm_map(doc,PlainTextDocument )

moviec.clean<-doc


##document term matrix


moviedtm<-DocumentTermMatrix(doc)
dim(moviedtm)

##Partitioning the Data

movie.train <- movie[1:1500,]
movie.test <- movie[1501:2000,]

moviedtm.train <- moviedtm[1:1500,]
moviedtm.test <- moviedtm[1501:2000,]

moviec.clean.train <- moviec.clean[1:1500]
moviec.clean.test <- moviec.clean[1501:2000]


## filtering the data set

dim(moviedtm.train)

fivefreq <- findFreqTerms(moviedtm.train, 95)
length((fivefreq))


dtm.train.fw <- DocumentTermMatrix(moviec.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.fw)


dtm.test.fw <- DocumentTermMatrix(moviec.clean.test, control=list(dictionary = fivefreq))

dim(dtm.test.fw)



################### Bayesian##################

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

trainNB <- apply(dtm.train.fw, 2, convert_count)
testNB <- apply(dtm.test.fw, 2, convert_count)

## training naive bayesian classifier

system.time( classifier <- naiveBayes(trainNB, movie.train$label) )

system.time( pred <- predict(classifier, newdata=testNB) )

table("Predictions"= pred,  "Actual" = movie.test$label )

confmatrix <- confusionMatrix(pred, movie.test$label)

confmatrix
