####Benjamin Arancibia
####IS621

#Week 3

#1

df1 <- read.csv("/users/bcarancibia/CUNY_IS_621/Week3/data/sample-training-data-naive-bayes.csv")
df2 <- data.table(read.table('/users/bcarancibia/CUNY_IS_621/Week3/data/sample-testing-data-naive-bayes.csv', header=TRUE))

df$Cat1 <- ifelse(df$Cat1.numeric=="A",1,-1)
inputs <- as.matrix(df)
#1)

require(data.table)

train <- function(dt){
  return(dt[,list(meancat1=mean(Cat1), varcat1=sd(Cat1),
                  meancat2=mean(Cat2), varcat2=sd(Cat2),
                  meancat3=mean(Cat3), varFootsize=sd(Cat3)), by=df$Class])
}
classify <- function (classifier, sample) {
  class <- function (sample, class_prior, Class) {
    p_cat1 <- dnorm(sample$Cat1, class$meancat1, class$varcat1)
    p_cat2 <- dnorm(sample$Cat2, class$meancat2, class$varcat2)
    p_cat3 <- dnorm(sample$Cat3, class$meancat3, class$varcat3)
    return(class_prior * p_cat1 * p_cat2 * p_cat3)
  }
  
  class_1 <- classifier[which(classifier$Class == 1),]
  class_neg_1 <- classifier[which(classifier$Class == -1),]
  
  prior_1 <- 0.5
  prior_neg_1 <- 0.5
  
  return(list(one=posterior(sample, prior_1, class_1),
              neg1=posterior(sample, prior_neg_1, class_neg_1)))
}

training_set <- data.table(read.table('/users/bcarancibia/CUNY_IS_621/Week3/data/sample-training-data-naive-bayes.csv', header=TRUE))
test_set <- data.table(read.table('/users/bcarancibia/CUNY_IS_621/Week3/data/sample-testing-data-naive-bayes.csv', header=TRUE))

sample = data.table(Cat1='B', Cat2='H', Cat3='Z')

classifier <- train(training_set)
result = classify(classifier, sample)

cat('posterior(class_1) =', result$class_1)
cat('posterior(class_neg_1) =', result$class_neg_1)


######
#had trouble with above below is to check how I did
######


require(e1071)

classifier <- naiveBayes(Class ~ Cat1 + Cat2 + Cat3, data = df)
test <- unique(df)
test$Prediction <- predict(classifier, test)
test
  

#2) 

df3 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/sample-testing-data-nearest-neighbor.csv', header=TRUE)
df4 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/sample-training-data-nearest-neighbor.csv', header=TRUE)

k.nearest.neighbors <- function(i, distance.matrix, k = 5)
{
  ordered.neighbors <- order(distance.matrix[i, ])   # closest to row i in descending order.
  return(ordered.neighbors[2:(k + 1)])
}

k.nearest.neighbors(10, d, 10)
knn <- function(df, k = 5)
{
  distance <- as.matrix(dist(df[,1:2]))
  predictions <- rep(0, nrow(df))
  
  # For every point in the dataset
  for (i in 1:nrow(df))
  {
    indices <- k.nearest.neighbors(i, distance, k = k)  # Get the nearest neighbors
    predictions[i] <- ifelse(mean(df[indices, 'Class']) > 0.5, 1, -1)   # If > 0.5, then this is in class "1"
  }
  return(predictions)
}

predictions <- knn(df4, 5)
df4$predictions <- predictions
sum(df4$Class != df4$predictions)

#Testing

predictions <- knn(df2, 5)
df2$predictions <- predictions
sum(df2$Class != df2$predictions)


#3 

#####
#See Azure ML learning website
####


#4

require(e1071)

classifier <- naiveBayes(Class ~ Cat1 + Cat2 + Cat3, data = df1)
test <- unique(df)
test$Prediction <- predict(classifier, test)
test

classifier <- naiveBayes(Class ~ Cat1 + Cat2 + Cat3, data = df2)
test <- unique(df2)
test$Prediction <- predict(classifier, test)
test

#####
The results are not identical to my results. 
####

require(class)

knn_predict <- knn(df3, k=5)

####
#The class package, as any nearest neighbors algorithm, depends on the k value picked. I think the knn class works faster than my implemented algorithm.
#The results were not the exact same
####

#5

require(e1071)

df5 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/jury-training-data.csv', header=TRUE)
df6 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/jury-learning-data-public.csv', header=TRUE)
df7 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/jury-learning-data-private.csv', header=TRUE)

classifier <- naiveBayes(tendency ~ agegroup + employment + gender + marital, data = df5)
test <- unique(df7)
test$Prediction <- predict(classifier, test)
table(test)

#based on the classifier, the following is predicted on the private dataset:

summary(test)

#It seems that , , gender = Female, marital = Married, Prediction = Guilty has the largest influencer on the prediction values.

#6

df8 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/pima-training-data.csv', header=TRUE)
df9 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/pima-learning-data-public.csv', header=TRUE)
df10 <- read.csv('/users/bcarancibia/CUNY_IS_621/Week3/data/pima-learning-data-private.csv', header=TRUE)

lapply(df8, as.numeric)

lapply(df9, as.numeric)

knn <- function(df, k = 5)
{
  distance <- as.matrix(dist(df[,1:2]))
  predictions <- rep(0, nrow(df))
  
  # For every point in the dataset
  for (i in 1:nrow(df))
  {
    indices <- k.nearest.neighbors(i, distance, k = k)  # Get the nearest neighbors
    predictions[i] <- ifelse(mean(df[indices, 'class']) > 0.5, 1, 0)   # If > 0.5, then this is in class "1"
  }
  return(predictions)
}

predictions <- knn(df8, 5)
df8$predictions <- predictions
sum(df8$class != df8$predictions)

knn_predict2 <- knn(df8, k=8)
sum(knn_predict2 != df8$class)

#My Analysis shows that my values that do not match are equal at 106
#Which is alright for 400 values, but this level of values not matching means the k values need to change.

predictions <- knn(df8, 8)
df8$predictions <- predictions
sum(df8$class != df8$predictions)

knn_predict2 <- knn(df8, k=8)
sum(knn_predict2 != df8$class)

#k=8 reduces the values of mismatch predictions to 96



