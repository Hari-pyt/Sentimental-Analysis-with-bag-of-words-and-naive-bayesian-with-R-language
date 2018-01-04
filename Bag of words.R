## loading libraries


library(tm)
library(wordcloud)
library(caret)
library(RTextTools)
library(e1071)
library(dplyr)
library(SnowballC)
library(wordcloud)



### reading positive and negative words
setwd("C:/Users/ghari/Desktop/TermPaper")

neg<-read.csv("neg.csv",header = T,stringsAsFactors = F)
negc<-Corpus(VectorSource(neg))
negc<-tm_map(negc,stemDocument,language="english")
negdtm<-DocumentTermMatrix(negc)
negwords<-findFreqTerms(negdtm)
str(negwords)
## we have a vector of stem negative words

## reading positive words
pos<-read.csv("pos.csv",header = T,stringsAsFactors = F)
posc<-Corpus(VectorSource(pos))
posc<-tm_map(posc,stemDocument,language="english")
posdtm<-DocumentTermMatrix(posc)
poswords<-findFreqTerms(posdtm)
str(poswords)

## we have a vector of stem negative words

## reading the file

movie<-read.csv("movie.csv",header = T,stringsAsFactors = F)

## we can take feedback directly from internet too
# read_html

summary(movie)
moviec<-Corpus(VectorSource(movie$review))  #converting corpus
dim(moviec)
length(moviec)


######cleaning the DATA
doc<-tm_map(moviec,removeNumbers)
doc<-tm_map(doc,removeWords,stopwords("english"))
doc<-tm_map(doc,removePunctuation)
doc<-tm_map(doc,stripWhitespace)
tempp<-doc
doc<-tm_map(doc,stemDocument,language="english")


##
#doc<-tm_map(doc,PlainTextDocument )
##
## creating document term matrix

moviedtm<-DocumentTermMatrix(doc)
dim(moviedtm)
# storing dtm in temp
temp<-moviedtm

## removing sparse words
##movies<-removeSparseTerms(moviedtm,0.8)

rr<-removeSparseTerms(moviedtm,0.8)
dim(rr)
mean_movie<-sort(colMeans(as.matrix(rr),na.rm = TRUE),decreasing = T)
mean_movie[1:20]
## top frequency words

### barplot
barplot(mean_movie[1:20],xlab="top 20 words",ylab="frequency",las=3,ylim=c(0,3))
## wordcloud
wordcloud(names(mean_movie[1:100]), mean_movie[1:100], min.freq = 0,scale=c(2,1)
          ,max.words=200, random.order=FALSE, rot.per=0.80,
          colors=brewer.pal(8, "Dark2"))


### removing unwanted words which are not helpful for decision
unwanted<-c("film","movie")
unwanted<-tm_map(tempp,removeWords,unwanted)
unwanted<-tm_map(unwanted,stemDocument,language="english")

moviedtm<-DocumentTermMatrix(unwanted)
dim(moviedtm)
rr<-removeSparseTerms(moviedtm,0.8)
dim(rr)
mean_movie<-sort(colMeans(as.matrix(rr),na.rm = TRUE),decreasing = T)
mean_movie[1:20]
## barplot
barplot(mean_movie[1:20],xlab="top 20 words",ylab="frequency",las=3,ylim=c(0,3))
## wordcloud
wordcloud(names(mean_movie[1:100]), mean_movie[1:100], min.freq = 0,scale=c(2,1)
          ,max.words=210, random.order=FALSE, rot.per=0.80,
          colors=brewer.pal(8, "Dark2"))

### visualization of high frequency words completed 

##Now  caluculating number of positive words
## we are creating a DTM with postivie words
posdtm<-DocumentTermMatrix(doc, control=list(dictionary=poswords))
dim(posdtm)
posreviewrow<-rowSums(as.matrix(posdtm),na.rm = TRUE)
dim(posreviewrow)

## we are creating a DTM with negative words
negdtm<-DocumentTermMatrix(doc, control=list(dictionary=negwords))
dim(negdtm)
negreviewrow<-rowSums(as.matrix(negdtm),na.rm = TRUE)
x<-length(negreviewrow)

positive<-0
negative<-0
neutral<-0



hari<-function()
{
  ### movie rating
  
 
  
  
  
  x<-length(negreviewrow)
  positive<-0
  negative<-0
  neutral<-0
  for( i in 1:x)
  {
    if( (posreviewrow[i]-negreviewrow[i])==0)
    { neutral<-neutral+1
    }
    if( (posreviewrow[i]-negreviewrow[i])>0)
    {positive<-positive+1 }
    else
    {
      if( (posreviewrow[i]-negreviewrow[i])<0)
      { negative<-negative+1} }}
  cat("\nthe number of positive review ", positive)
  cat("\nthe number of negative review",negative)
  cat("\nthe number of neutral reviews",neutral)
 
  }
hari()

