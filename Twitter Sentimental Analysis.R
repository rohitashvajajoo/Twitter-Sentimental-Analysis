

#### social media analytics ####

library(twitteR)
# you need to set up your own twitter account and obtain the keys first!

fitTweets<-searchTwitter("$fit",n=200)
feyeTweets <-searchTwitter("$feye", since='2017-02-01', until='2020-02-10')
babaTweets<-searchTwitter('$baba', geocode='32.882649,-96.763419,100mi') #UTD geocode
#hardvard 42.375,-71.1061111,100mi

library(plyr); library(ggplot2)
fitTweets2.df = ldply(fitTweets, function(t) t$toDataFrame())
fit_table <- twListToDF(fitTweets)
#tweets <- fit_table


tweets <- read.csv("fitbit18.csv", stringsAsFactors = FALSE)
library(scales);  library(reshape2)
library(NLP); library(openNLP) 
#library(openNLPmodels.en)
library(tm); library(stringr); library(gsubfn); library(plyr); library(dplyr)
library(lubridate); library(rlang); library(ggplot2)

#count number of tweets
tweets$timestamp <- ymd_hms(tweets$created)
#tweets$timestamp <- with_tz(tweets$timestamp, "America/Chicago")
ggplot(data = tweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

#wordcloud, sentiment analysis etc.


###library(plyr)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(stringr)
list<-read.table("fitbit18.csv", sep="\t")
head(list, n=5)
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('C:/Users/zxz062000/Desktop/backup1228/finance/FinancialAnalytics/R-TextAnalytics/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:/Users/zxz062000/Desktop/backup1228/finance/FinancialAnalytics/R-TextAnalytics/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'hack', 'breach', 'cybersecurity')
Dataset <- list
Dataset$text <- as.factor(Dataset[,1])
scores <- score.sentiment(Dataset$text, pos.words, neg.words)
scores$score

library(syuzhet)
tweets <- read.csv("fitbit18.csv", stringsAsFactors = FALSE) #data needs to be a dataframe
mySentiment <- get_nrc_sentiment(tweets$text)
head(mySentiment)
tweets <- cbind(tweets, mySentiment)
sentimentTotals <- data.frame(colSums(tweets[,c(18:27)])) 
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

### better, newer alternative package sentimentR

library(sentimentr)
tweets <- read.csv("fitbit18.csv", stringsAsFactors = FALSE)
mytext <- tweets$text
scores <- sentiment(mytext)

mytext <- get_sentences(mytext) #sentense level sentiment
sentiment_by(mytext) #group by sentences


###text mining and NLP####

library(tm); library(twitteR); library(NLP)

fitTweets <- read.csv("fitbit18.csv", stringsAsFactors = FALSE)
head(fitTweets, n=5)
str(fitTweets)
#df <- do.call("rbind", lapply(rdmTweets, as.data.frame))


#build a corpus, use vectorsouce to specify character sources
myCorpus <- Corpus(VectorSource(fitTweets$text))

# step 1, tranform the text, e.g. making all words lowercase, ermoving punctuations, stop words.
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
inspect(myCorpus[1:3]) 
myStopwords <- c(stopwords('english'), "available", "via")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
summary(myStopwords)

## step2: dealing with stemming
dictCorpus <- myCorpus
library(SnowballC); library(RWeka); library(RWekajars); library(rJava)
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:3])

#step 3: create the term frequency table
#myCorpus <- Corpus(VectorSource(rdmTweets$text))  
myDtm <- DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

idx <- which(dimnames(myTdm)$Terms == "fit")
inspect(myTdm[idx+(0:5),1:10])

#step 4: frequent terms and associations
findFreqTerms(myTdm, lowfreq=10)
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
barplot(termFrequency, las=2)
findAssocs(myTdm,'fit',0.1)
findAssocs(myTdm, 'wearable', 0.1)

#step 5: word cloud/tag cloud
library(wordcloud)
m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m),decreasing=TRUE)
set.seed(375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)

#step 6. Clustering words using hierachical clustering
myTdm2 <- removeSparseTerms(myTdm, sparse=0.95)
m2 <- as.matrix(myTdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward")
plot(fit)
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))

# step 7: cluster tweets using k-means
m3 <- t(m2) #transpose
set.seed(122)
k <- 8
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3)

for (i in 1:k) {
  cat(paste("cluster ", i, ":  ", sep=""))
  s <- sort(kmeansResult$centers[i,])
  cat(names(s)[1:3], "\n")
  # print the tweets of every cluster
}


## step 8 topic modleing
library(topicmodels)
set.seed(123)
myLda <- LDA(as.DocumentTermMatrix(myTdm), k=18)
terms(myLda, 10)




