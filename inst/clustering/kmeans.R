#This script was used to create the countries cluster that is included with the package

countries_data <- read.csv("inst/clustering/Country.csv", header = TRUE, stringsAsFactors = FALSE)

library(ggplot2)
ggplot(countries_data, aes(Per_capita_income, Infant_mortality, color = Country)) + geom_point()

set.seed(1234)
countries_cluster <- kmeans(countries_data[,2:6], 3, nstart = 15)
print(countries_cluster)
table(countries_cluster$cluster, countries_data$Country)

save(countries_data, file="data/countries_data.rda")
save(countries_cluster, file="data/countries_cluster.rda")

countries_cluster$cluster <- as.factor(countries_cluster$cluster)
ggplot(countries_data, aes(Per_capita_income, Life_expectancy, label = Country, color = countries_cluster$cluster)) +
  geom_label()
