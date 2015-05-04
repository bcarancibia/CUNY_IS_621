#Ben Arancibia
Classification Model Metrics

# 1) read in the csv

data <- read.csv("/users/bcarancibia/CUNY_IS_621/classificationmethods/classificationoutput.csv")
View(data)

#2

table(data$class,data$Scored.Labels)

