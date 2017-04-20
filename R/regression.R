
getCarPrice <- function(input)
  {
  car <- as.data.frame(input)

  stopifnot("Brand" %in% names(car))
  stopifnot("Model" %in% names(car))
  stopifnot("Year" %in% names(car))
  stopifnot("Engine" %in% names(car))
  stopifnot("Kilometers" %in% names(car))
  stopifnot("Diesel" %in% names(car))
  stopifnot("Horsepower" %in% names(car))
  stopifnot("Euro" %in% names(car))

  list(carPrice =  predict.lm(regression_model, car))
  }

