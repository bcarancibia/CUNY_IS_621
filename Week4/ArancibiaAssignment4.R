#Benjamin Arancibia
#Assignment 4

data.private <- read.table(file="/users/bcarancibia/CUNY_IS_621/Week4/jury-learning-data-private.csv", header = TRUE, sep=",", stringsAsFactors=FALSE)
data.public <- read.table(file="/users/bcarancibia/CUNY_IS_621/Week4/jury-learning-data-public.csv", header = TRUE, sep=",", stringsAsFactors=FALSE)
training.data <- read.table(file="/users/bcarancibia/CUNY_IS_621/Week4/jury-training-data.csv", header = TRUE, sep=",", stringsAsFactors=FALSE)

require(plyr)

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

entropy(training.data$tendency)
information_gain(training.data$tendency, training.data$agegroup)
information_gain(training.data$tendency, training.data$employment)
information_gain(training.data$tendency,training.data$gender)
information_gain(training.data$tendency,training.data$marital)
decide(training.data,5)

entropy(data.public$tendency)
information_gain(data.public$tendency, data.public$agegroup)
information_gain(data.public$tendency, data.public$employment)
information_gain(data.public$tendency,data.public$gender)
information_gain(data.public$tendency,data.public$marital)
decide(data.public,5)






