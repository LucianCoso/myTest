#This script was used to create the regression model that is included with the package

# Reading data
carsData <- read.csv("inst/regression/CarsData.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)

# Inspecting data
str(carsData)
summary(carsData$Price)
hist(carsData$Price)
table(carsData$Model)
cor(carsData[c("Year", "Kilometers", "Horsepower", "Price")])
pairs((carsData[c("Year", "Kilometers", "Horsepower", "Price")]))

# Creating the model
library(caret)
set.seed(1234)
inTrainRows <- createDataPartition(carsData$Model, p=0.70, list = FALSE)
carsTrain <-carsData[inTrainRows,]
carsTest <-carsData[-inTrainRows,]
regression_model <- lm(Price~., carsData)

# Evaluating performances
regression_model
summary(regression_model)
predict(regression_model, carsTest)
carsTest$Price

# Saving the model
save(regression_model, file="data/regression_model.rda")
