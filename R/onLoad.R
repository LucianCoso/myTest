
.onLoad <- function(lib, pkg){
  #automatically loads the dataset when package is loaded
  #do not use this in combination with lazydata=true
  utils::data(regression_model, package = pkg, envir = parent.env(environment()))
  utils::data(cardio_data, package = pkg, envir = parent.env(environment()))
  utils::data(countries_data, package = pkg, envir = parent.env(environment()))
  utils::data(transaction_data, package = pkg, envir = parent.env(environment()))
  utils::data(transaction_rules, package = pkg, envir = parent.env(environment()))
  utils::data(comments, package = pkg, envir = parent.env(environment()))
}

