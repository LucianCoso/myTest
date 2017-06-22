
getDiagnostic <- function(patient)
  {
  patienData <- as.data.frame(patient)

  stopifnot("LB" %in% names(patienData))
  stopifnot("AC" %in% names(patienData))
  stopifnot("FM" %in% names(patienData))
  stopifnot("UC" %in% names(patienData))
  stopifnot("DL" %in% names(patienData))
  stopifnot("ASTV" %in% names(patienData))
  stopifnot("MSTV" %in% names(patienData))
  stopifnot("ALTV" %in% names(patienData))
  stopifnot("MLTV" %in% names(patienData))
  stopifnot("Width" %in% names(patienData))
  stopifnot("Min" %in% names(patienData))
  stopifnot("Max" %in% names(patienData))
  stopifnot("Nmax" %in% names(patienData))
  stopifnot("Nzeros" %in% names(patienData))
  stopifnot("Mode" %in% names(patienData))
  stopifnot("Mean" %in% names(patienData))
  stopifnot("Median" %in% names(patienData))
  stopifnot("Variance" %in% names(patienData))
  stopifnot("Tendency" %in% names(patienData))

  cardio_train<-cardio_data[,1:19]
  cardio_train_labels<-cardio_data[,20]

  library(class)
  list(diagnostic = knn(train = cardio_train, test = patienData, cl = cardio_train_labels, k = 13))
  }

