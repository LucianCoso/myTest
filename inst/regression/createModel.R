#This script was used to create the regression model that is included with the package

carsData <- read.csv("inst/regression/CarsData.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
library(caret)
set.seed(1234)
inTrainRows <- createDataPartition(carsData$Model, p=0.70, list = FALSE)
carsTrain <-carsData[inTrainRows,]
carsTest <-carsData[-inTrainRows,]
regression_model <- lm(Price~., carsTrain)
save(regression_model, file="data/regression_model.rda")
