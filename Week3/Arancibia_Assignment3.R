####Benjamin Arancibia
####IS621

#Week 3

df <- read.csv("/users/bcarancibia/CUNY_IS_621/Week3/data/sample-training-data-naive-bayes.csv")
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
