library(randomForest)
library(RCurl)
library(dplyr)
library(gplots)

data <- read.csv(
  text = getURL('https://raw.githubusercontent.com/FrancaSp/DataScience_Project/master/Datensaetze/finale_daten.csv')
)
data <- data[2:ncol(data)] # Remove var "X1" from Data
data$RateofChange <- NA # initialize Target 

summary(data)

index <- c(1:145)

# Set traget Values in this case for two classes 
for (i in index){
  if (data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 0.5))
  {
    data$RateofChange[i]<- 1 
  }
  else
  {
    data$RateofChange[i]<- 2
  }
}

data <- data[-10] # delete HousePrice.RateofChange from Data

data$RateofChange <- as.factor(data$RateofChange) # convert to facor for analysis

library(caret)
# Create features and target
data <- na.roughfix(data) # set median for missing values
X <- data %>% 
  select(Overall.rank, GDP.per.capita, #Score,
           Healthy.life.expectancy, Freedom.to.make.life.choices,
           Generosity, Perceptions.of.corruption,  EmploymentRate, Job.Vacancy, #longterm_interestrate,
         HousePrice.AvgIndex.oneYearago, HousePrice.AvgIndex.twoYearsago)
y <- data$RateofChange

# Split data into training and test sets
set.seed(14)
index <- caret::createDataPartition(y, p=0.75, list=FALSE)
X_train <- X[ index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test <- y[-index]

# Train the model 
regr2 <- randomForest(x = X_train, y = y_train, maxnodes = 100, ntree = 100, replace = TRUE)

# Make prediction
predictions <- predict(regr2, X_test)

#result <- X_test
#result['prediction'] <- y_test
#result['prediction']<-  predictions
#head(result)

table(predictions,y_test) 
confusionMatrix(predictions,y_test)
regr2$importance
varImpPlot(regr2)

pred1=predict(regr2,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], y_train)
# 1. Area under curve
auc = performance(perf, "auc")
auc
# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
