
getPlot <- function(nrClusters, xaxis, yaxis)
  {
  set.seed(1234)
  countries_cluster <- kmeans(countries_data[,2:6], nrClusters, nstart = 15)
  countries_cluster$cluster <- as.factor(countries_cluster$cluster)

  library(ggplot2)
  plot <- ggplot(countries_data, aes(get(xaxis), get(yaxis), label = Country, color = countries_cluster$cluster)) +
    geom_label() + labs(title = "Countries cluster", x = toString(xaxis), y = toString(yaxis))
  print(plot);
  }

changeNrClusters <- function(nrClusters)
  {
  set.seed(1234)
  countries_cluster <- kmeans(countries_data[,2:6], nrClusters, nstart = 15)
  save(countries_cluster, file="data/countries_cluster.rda")
  }


getClusters <- function(nrClusters)
  {
  set.seed(1234)
  countries_cluster <- kmeans(countries_data[,2:6], nrClusters, nstart = 15)
  list(clusters = countries_cluster$cluster);
  }
