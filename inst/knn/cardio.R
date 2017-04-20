#This script was used to create the knn classifier that is included with the package

cardio <- read.csv("inst/knn/Cardio.csv", stringsAsFactors = FALSE)
cardio$DS <- NULL
cardio$DP <- NULL
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
cardio_norm <- as.data.frame(lapply(cardio[1:20], normalize))

cardio_norm$NSP <- factor(cardio_norm$NSP, levels = c(0.0, 0.5, 1.0), labels = c("Normal","Suspect", "Pathologic"))
barplot(table(cardio_norm$NSP), main="Categories", col="black")

library(caret)
set.seed(1234)
inTrainRows <- createDataPartition(cardio_norm$NSP, p=0.70, list = FALSE)
cardio_train <- cardio_norm[inTrainRows, 1:19]
cardio_test <- cardio_norm[-inTrainRows, 1:19]
cardio_train_labels <- cardio_norm[inTrainRows, 20]
cardio_test_labels <- cardio_norm[-inTrainRows, 20]

library(class)
library(gmodels)
cardio_model <- knn(train = cardio_train, test = cardio_test, cl = cardio_train_labels, k = 13)
CrossTable(x = cardio_test_labels, y = cardio_model, prop.chisq = FALSE)

knn(train = cardio_train, test = cardio_test[1,1:19], cl = cardio_train_labels, k = 13)

cardio_data <- cardio_norm[inTrainRows, 1, 20]
save(cardio_data, file="data/cardio_data.rda")
