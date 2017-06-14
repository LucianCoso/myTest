library("arules")
transaction_data <- read.transactions("inst/asociationRules/Groceries.csv", sep = ",")
summary(transaction_data)
inspect(transaction_data[1:10])
itemFrequency(transaction_data[ ,1:10])
itemFrequencyPlot(transaction_data, support = 0.1)
itemFrequencyPlot(transaction_data, topN = 40)

transaction_rules <- apriori(transaction_data, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
save(transaction_data, file="data/transaction_data.rda")
save(transaction_rules, file="data/transaction_rules.rda")

summary(transaction_rules)
inspect(transaction_rules[1:5])
inspect(sort(transaction_rules, by = "lift")[1:5])
subset_rule <- subset(transaction_rules, lhs %ain% c("soda", "sausage"))
subset_rule <- subset(transaction_rules, lhs %ain% c("yogurt"))
unique(inspect(sort(subset_rule, by = "lift")))

mytest <- function(test)
{
  test <-unlist(strsplit(test, ','))
  length(test)
  item1 <- test[1]
  item2 <- test[2]
  item3 <- test[3]
  item1
  item2
  item3

  if(!is.na(item1) && !is.na(item2) && !is.na(item3)){
    subset_rule <- subset(transaction_rules, lhs %ain% c(item1, item2, item3))
  } else if(!is.na(item1) && !is.na(item2)){
    subset_rule <- subset(transaction_rules, lhs %ain% c(item1, item2))
  } else if(!is.na(item1)) {
    subset_rule <- subset(transaction_rules, lhs %in% item1)
  }

  if(length(subset_rule) == 0)
  {
    if(!is.na(item1) && !is.na(item2) && !is.na(item3)){
      subset_rule <- subset(transaction_rules, lhs %in% c(item1, item2, item3))
    } else if(!is.na(item1) && !is.na(item2)){
      subset_rule <- subset(transaction_rules, lhs %in% c(item1, item2))
    } else if(!is.na(item1)) {
      subset_rule <- subset(transaction_rules, lhs %in% c(item1))
    }
  }

  inspect(sort(subset_rule, by = "lift"))

  subset_rule <- sort(subset_rule, by = "lift")
  distinct_rhs <- as(rhs(subset_rule), "list")
  distinct_rhs <- unique(distinct_rhs)
  if(length(distinct_rhs) > 3)
    distinct_rhs <- distinct_rhs[1:3]
  distinct_rhs

}
mytest("soda")

myTest2 <- function(){
  subset_rule <- apriori(transaction_data, parameter = list(support = 0.006, confidence = 0.25, minlen = 2, target="rules"), appearance = list(lhs = c(item1, item2), default="rhs"))
  out <- capture.output(inspect(sort(subset_rule, by = "lift")))
  distinct_rhs <- gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
  if(length(distinct_rhs) > 3)
    distinct_rhs <- unique(distinct_rhs)[1:3]
  distinct_rhs
}
myTest2()
