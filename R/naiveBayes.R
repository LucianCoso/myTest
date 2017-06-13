replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}

convertCounts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

isSpam <- function(input) {
  comments[1957,]$CONTENT <- input
  comments[1957,]$CLASS <- 1

  commentsCorpus <- VCorpus(VectorSource(comments$CONTENT))
  commentsCorpus <- tm_map(commentsCorpus, content_transformer(tolower))
  commentsCorpus <- tm_map(commentsCorpus, removeNumbers)
  commentsCorpus <- tm_map(commentsCorpus, removeWords, stopwords())
  commentsCorpus <- tm_map(commentsCorpus, content_transformer(replacePunctuation))
  commentsCorpus <- tm_map(commentsCorpus, stripWhitespace)
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
  list(result = predict(commentsClassifier, commentsTest)[2])
}

