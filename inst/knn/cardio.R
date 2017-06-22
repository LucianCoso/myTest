#This script was used to create the knn classifier that is included with the package

# Import data
cardio <- read.csv("inst/knn/Cardio.csv", stringsAsFactors = FALSE)
cardio$DS
cardio$DP
cardio$DS <- NULL
cardio$DP <- NULL
table(cardio$NSP)

# Data frame normalization
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
cardioNorm <- as.data.frame(lapply(cardio[1:20], normalize))

# Set target value as factor
cardio$NSP <- factor(cardio$NSP, levels = c(1, 2, 3), labels = c("Normal","Suspect", "Pathologic"))
cardioNorm$NSP <- factor(cardioNorm$NSP, levels = c(0.0, 0.5, 1.0), labels = c("Normal","Suspect", "Pathologic"))
barplot(table(cardio$NSP), main="Categories", col="black")
barplot(table(cardioNorm$NSP), main="Categories", col="black")

# Split data into train and test
library(caret)
set.seed(1234)
inTrainRows <- createDataPartition(cardioNorm$NSP, p=0.70, list = FALSE)
cardioTrain <- cardioNorm[inTrainRows, 1:19]
cardioTest <- cardioNorm[-inTrainRows, 1:19]
cardioTrainLabels <- cardioNorm[inTrainRows, 20]
cardioTestLabels <- cardioNorm[-inTrainRows, 20]

# Apply knn from class library
library(class)
cardioModel <- knn(train = cardioTrain, test = cardioTest, cl = cardioTrainLabels, k = 13)

# Verify the results
library(gmodels)
CrossTable(x = cardioTestLabels, y = cardioModel, prop.chisq = FALSE)

# Test
knn(train = cardioTrain, test = cardioTest[1,1:19], cl = cardioTrainLabels, k = 13)

# Save cardio data
cardio_data <- cardioNorm[inTrainRows, 1:20]
cardio_data
save(cardio_data, file="data/cardio_data.rda")
