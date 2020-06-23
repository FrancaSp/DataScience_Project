##### Decision Tree - C5.0 Algorithm #####


# Loading the packages:
library(RCurl)
library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ipred)
library(C50)



### (1) Loading the data:
data_all <- read.csv(
  text = getURL('https://raw.githubusercontent.com/FrancaSp/DataScience_Project/master/Datensaetze/finale_daten.csv')
 )

data_all <- data_all[2:ncol(data_all)] # Remove column "X1"



### (2) Divide the dependent variable into the classes:
data_all$RateofChange <- NA # Creating a new column "RateofChange" for the classifications
                            # (This variable is used as the dependent variable)

index <- c(1:145)
for (i in index){
  if (data_all$HousePrice.RateofChange[i] <= quantile(data_all$HousePrice.RateofChange, 0.5))
  {
    data_all$RateofChange[i] <- 1 # Class for data with a low rate of change in house prices
  }
  if (data_all$HousePrice.RateofChange[i] > quantile(data_all$HousePrice.RateofChange, 0.5))
  {
    data_all$RateofChange[i] <- 2 # Class for data with a high rate of change in house prices
  }
}
data_all <- data_all[-10] 
data_all$RateofChange <- as.factor(data_all$RateofChange)   



### (3) Split the data into training and test dataset:
set.seed(12) # Set value for seed (for reproducibility)

split_id <- sample(1:nrow(data_all), size = floor(0.75 * nrow(data_all)), replace=FALSE) # 75% training
data_train <- data_all[split_id,] # Dataset for training
data_test <- data_all[-split_id,] # Dataset for testing



### (4) C5.0 Classification Tree:
model <- RateofChange ~ Score + Overall.rank + GDP.per.capita + Healthy.life.expectancy + 
  Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Job.Vacancy + 
  EmploymentRate + HousePrice.AvgIndex.oneYearago + HousePrice.AvgIndex.twoYearsago
  # The model uses all independent variables, except "longterm_interestrate"

tree_C50 <- C5.0(x=data_train, y=as.factor(data_train$RateofChange))
tree_C50 <- C5.0(model, data=data_train, trials=50)

#summary(tree_C50) # Possibility to display the summary of the tree
plot(tree_C50) # Create a plot of the tree



### (5) Predicting the classes of the test dataset:
pred <- predict(tree_C50, data_test)



### (6) Comparison of predicted class and actual class of test dataset by using a Confusion Matrix:
data_test$RateofChange <- as.factor(data_test$RateofChange)
confusionMatrix(pred, as.factor(data_test$RateofChange), dnn = c("Prediction", "Actual Data"))

