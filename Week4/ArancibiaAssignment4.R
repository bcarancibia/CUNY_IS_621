#Benjamin Arancibia
#Assignment 4

# Calculate Entropy for a vector
entropy <- function(input){
  observations <- length(input)
  table_test <- table(input)
  p <- table_test/observations
  sigma <- p*log2(p)
  sigma[is.na(sigma)] = 0
  entropy = -1 * sum(sigma)
  return (entropy)
}

#Implement a function that calculates the information gain of one categorical vector
information_gain <- function(target, attribute){
  observations <- length(attribute)
  split <- table(attribute)
  values <- names(split)
  values_observations <- length(values)
  probability <- split/observations
  target_entropy <- entropy(target)
  attribute_entropy <- 0
  #create splice of the values and find the probability
  #partition
  for (i in 1:values_observations){
    attribute_entropy <- attribute_entropy + probability[i] * entropy(target[attribute==values[i]])
  }
  return (target_entropy - attribute_entropy)  
}

#Implement a function that takes input of dataframe of categorical variables
decide<-function(df,target){
  target_entropy <- entropy(df[,target])
  attribute_entropy <- apply(df[,-target], 2, function(x) information_gain(df[, target], x))
  return(list(max.attr = which(attribute_entropy == max(attribute_entropy)),
              information_gain = attribute_entropy))
}

