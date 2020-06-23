##### Decision Tree - CART Algorithm #####


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

train_id <- sample(1:nrow(data_all), size = floor(0.75 * nrow(data_all)), replace=FALSE) # 75% training
data_train <- data_all[train_id,] # Dataset for training
data_test <- data_all[-train_id,] # Dataset for testing



### (4) CART Classification Tree:
model <- RateofChange ~ Score + Overall.rank + GDP.per.capita + Healthy.life.expectancy + 
  Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Job.Vacancy + 
  EmploymentRate + HousePrice.AvgIndex.oneYearago + HousePrice.AvgIndex.twoYearsago
  # The model uses all independent variables, except "longterm_interestrate"

tree <- rpart (formula = model , # Previously defined model with the dependent variable and the independent variables
               data = data_train, # Dataset for training the algorithm
               na.action = na.rpart, # Observations, where all explanatory variables are missing, are deleted
               method = "class", # Defines the decision problem (In our projekt: "classification") 
               parms = list(split = 'gini'), # Using the Gini Index as the splitting criterion
               control = rpart.control(minsplit = 4, # Minimal number of observations in a node before a split is attempted
                                       cp = 0.01, # Complexity parameter: minimal increase in splitting criterion so that a split is attempted
                                       maxdepth = 4)) # Maximum depth of the tree


rpart.plot(tree,extra=101) # Create a plot of the tree
#summary(tree) # Possibility to display the summary of the tree
#printcp(tree) # Possibility to display the table of optimal prunings based on the complexity parameter
#tree$variable.importance # Possibility to display the importances of the independet variables



### (5) Predicting the classes of the test dataset:
pred <- predict(tree, newdata = data_test, type="class")
data_test_pred <- cbind(data_test, pred)



### (6) Comparison of predicted class and actual class of test dataset by using a Confusion Matrix:
data_test$RateofChange <- as.factor(data_test$RateofChange)
confusionMatrix(data_test_pred$pred, data_test_pred$RateofChange, 
                dnn = c("Prediction", "Actual Data"))

