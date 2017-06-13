#This script was used to create the naive Bayes classifier that is included with the package

# Import and clean data
comments <- read.csv("inst/naiveBayes/Youtube.csv", stringsAsFactors = FALSE)
comments$AUTHOR <- NULL
comments$DATE <- NULL
comments$CLASS <- factor(comments$CLASS)
comments
save(comments, file="data/comments.rda")

# Text minig
library(tm)
commentsCorpus <- VCorpus(VectorSource(comments$CONTENT))
lapply(commentsCorpus[1:18], as.character)

commentsCorpus <- tm_map(commentsCorpus, content_transformer(tolower))
commentsCorpus <- tm_map(commentsCorpus, removeNumbers)
commentsCorpus <- tm_map(commentsCorpus, removeWords, stopwords())

replacePunctuation <- function(x) { gsub("[[:punct:]]+", " ", x) }
commentsCorpus <- tm_map(commentsCorpus, content_transformer(replacePunctuation))

commentsCorpus <- tm_map(commentsCorpus, stripWhitespace)

library(SnowballC)
commentsCorpus <- tm_map(commentsCorpus, stemDocument)

# DTM creation
commentsDtm <- DocumentTermMatrix(commentsCorpus)
commentsDtmTrain <- commentsDtm[1:1500, ]
commentsDtmTest <- commentsDtm[1501:1956, ]
commentsLabelsTrain <- comments[1:1500, ]$CLASS
commentsLabelsTest <- comments[1501:1956, ]$CLASS

prop.table(table(commentsLabelsTrain))
prop.table(table(commentsLabelsTest))

# Words frequency
library(wordcloud)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(commentsCorpus, min.freq = 40, random.order = FALSE, colors = pal2)
commentsHam <- subset(comments, CLASS == 0)
commentsSpam <- subset(comments, CLASS == 1)
wordcloud(commentsHam$CONTENT, max.words = 80, random.order = FALSE, scale = c(3, 0.5), colors = pal2)
wordcloud(commentsSpam$CONTENT, max.words = 80, random.order = FALSE, scale = c(3, 0.5), colors = pal2)

commentsFreqWords <- findFreqTerms(commentsDtmTrain, 5)
commentsDtmFreqTrain <- commentsDtmTrain[ , commentsFreqWords]
commentsDtmFreqTest <- commentsDtmTest[ , commentsFreqWords]

convertCounts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

commentsTrain <- apply(commentsDtmFreqTrain, MARGIN = 2, convertCounts)
commentsTest <- apply(commentsDtmFreqTest, MARGIN = 2, convertCounts)

library(e1071)
commentsClassifier <- naiveBayes(commentsTrain, commentsLabelsTrain)

library(gmodels)
commentsPrediction <- predict(commentsClassifier, commentsTest)
CrossTable(commentsPrediction, commentsLabelsTest, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))


# Function for test
isSpam <- function(text) {
  comments[1957,]$CONTENT <- text
  comments[1957,]$CLASS <- 1

  commentsCorpus <- VCorpus(VectorSource(comments$CONTENT))
  commentsCorpus <- tm_map(commentsCorpus, content_transformer(tolower))
  commentsCorpus <- tm_map(commentsCorpus, removeNumbers)
  commentsCorpus <- tm_map(commentsCorpus, removeWords, stopwords())
  commentsCorpus <- tm_map(commentsCorpus, content_transformer(replacePunctuation))
  commentsCorpus <- tm_map(commentsCorpus, stripWhitespace)
  library(SnowballC)
  commentsCorpus <- tm_map(commentsCorpus, stemDocument)

  commentsDtm <- DocumentTermMatrix(commentsCorpus)
  commentsDtmTrain <- commentsDtm[1:1956, ]
  commentsDtmTest <- commentsDtm[1956:1957, ]
  commentsLabelsTrain <- comments[1:1956, ]$CLASS

  commentsFreqWords <- findFreqTerms(commentsDtmTrain, 5)
  commentsDtmFreqTrain <- commentsDtmTrain[ , commentsFreqWords]
  commentsDtmFreqTest <- commentsDtmTest[ , commentsFreqWords]

  commentsTrain <- apply(commentsDtmFreqTrain, MARGIN = 2, convertCounts)
  commentsTest <- apply(commentsDtmFreqTest, MARGIN = 2, convertCounts)

  commentsClassifier <- naiveBayes(commentsTrain, commentsLabelsTrain)
  predict(commentsClassifier, commentsTest)[2]
}

VCorpus(VectorSource("Adsasd"))

isSpam2("check my blog on https://myblog.com/asdasdaf")
