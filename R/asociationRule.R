
getRecommendations <- function(input)
{
  items <- unlist(strsplit(input, ','))
  item1 <- items[1]
  item2 <- items[2]
  item3 <- items[3]

  if(!is.na(item1) && !is.na(item2) && !is.na(item3)){
    subset_rule <- subset(transaction_rules, lhs %ain% c(item1, item2, item3))
  } else if(!is.na(item1) && !is.na(item2)){
    subset_rule <- subset(transaction_rules, lhs %ain% c(item1, item2))
  } else if(!is.na(item1)) {
    subset_rule <- subset(transaction_rules, lhs %in% c(item1))
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

  out <- capture.output(inspect(sort(subset_rule, by = "lift")))
  distinct_rhs <- gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
  distinct_rhs <- unique(distinct_rhs)
  if(length(distinct_rhs) > 3)
    distinct_rhs <- distinct_rhs[1:3]

  list(recommendations = distinct_rhs);
}
