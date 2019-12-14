setwd("~/Python Scripts")
Data = read.csv("Data.csv")

#tratamiento de los NA'S
Data$Age[is.na(Data$Age)] <- round(mean(Data$Age, na.rm = TRUE))
Data$Salary[is.na(Data$Salary)] <- round(mean(Data$Salary, na.rm = TRUE))


# otra forma
Data$Age = ifelse(is.na(Data$Age),
                     ave(Data$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     Data$Age)
Data$Salary = ifelse(is.na(Data$Salary),
                        ave(Data$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        Data$Salary)


#codificar las variables categoricas
Data$Country = factor(Data$Country, levels = c("France", "Spain", "Germany"),
                      labels = c(1, 2, 3))

Data$Purchased = factor(Data$Purchased,
                           levels = c("No", "Yes"),
                           labels = c(0,1))
str(Data)

#Dividir el data en train y test
install.packages("caTools")
library(caTools)
set.seed(123)

split = sample.split(Data$Purchased, SplitRatio = 0.8)
training_set = subset(Data, split == TRUE)
testing_set = subset(Data, split == FALSE)

#OTRA FORMA MEJOR
install.packages("caret")
library(caret)
in_train <- createDataPartition(Data$Purchased, p = 0.8, list = FALSE)
data_train <- Data[in_train, ]
data_test <- Data[-in_train, ]  
data.frame(training_set, data_train)
data.frame(testing_set, data_test)

# Escalado de valores
training_set[,2:3] = scale(training_set[,2:3])
testing_set[,2:3] = scale(testing_set[,2:3])

data_train[,2:3] = scale(data_train[,2:3])
data_test[,2:3] = scale(data_test[,2:3])
