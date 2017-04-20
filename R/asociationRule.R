
getRecommendations <- function(input)
{
  library("arules")

  items <- unlist(strsplit(input, ','))
  item1 <- toString(items[1])
  item2 <- toString(items[2])
  item3 <- toString(items[3])

  subset_rule <- apriori(transaction_data, parameter = list(support = 0.006, confidence = 0.25, minlen = 2, target="rules"), appearance = list(lhs = c(item1, item2), default="rhs"))
  #subset(transaction_rules)

  #if(!is.na(item1) && !is.na(item2) && !is.na(item3)){
  #  subset_rule <- subset(transaction_rules, lhs %ain% c(item1, item2, item3))
  #} else if(!is.na(item1) && !is.na(item2)){
  #  subset_rule <- subset(transaction_rules, lhs %ain% c(item1, item2))
  #} else if(!is.na(item1)) {
  #  subset_rule <- subset(transaction_rules, lhs %in% c(item1))
  #}

  #if(length(subset_rule) == 0)
  #{
  #  if(!is.na(item1) && !is.na(item2) && !is.na(item3)){
  #    subset_rule <- subset(transaction_rules, lhs %in% c(item1, item2, item3))
  #  } else if(!is.na(item1) && !is.na(item2)){
  #    subset_rule <- subset(transaction_rules, lhs %in% c(item1, item2))
  #  } else if(!is.na(item1)) {
  #    subset_rule <- subset(transaction_rules, lhs %in% c(item1))
  #  }
  #}

  out <- capture.output(inspect(sort(subset_rule, by = "lift")))
  #distinct_rhs <- gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
  #if(length(distinct_rhs) > 3)
  #  distinct_rhs <- unique(distinct_rhs)[1:3]

  list(recommendations = out);
  #list(recommendations = inputString);
}
