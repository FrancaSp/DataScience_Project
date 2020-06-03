library(randomForest)

data <- read.csv(
  text = getURL('https://raw.githubusercontent.com/FrancaSp/DataScience_Project/master/Datensaetze/finale_daten.csv')
)

data <- read.csv('~/Documents/UNI/Master/Data_Science/Daten_Codes/Datensaetze/finale_daten.csv')

data <- data[2:ncol(data)]

summary(data$HousePrice.RateofChange)
quantile(data$HousePrice.RateofChange, c(0.33, 0.66,  1))

data$RateofChange <- NA

index <- c(1:145)
for (i in index){
  if (data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 0.25))
  {
    data$RateofChange[i]<- 1
  }
  if (data$HousePrice.RateofChange[i] > quantile(data$HousePrice.RateofChange, 0.25) &
      data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 0.5))
  {
    data$RateofChange[i]<- 2
  }
  if (data$HousePrice.RateofChange[i] > quantile(data$HousePrice.RateofChange, 0.5) &
      data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 0.75))
  {
    data$RateofChange[i]<- 3
  }
  if (data$HousePrice.RateofChange[i] > quantile(data$HousePrice.RateofChange, 0.75) &
      data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 1))
  {
    data$RateofChange[i]<- 4
  }
}
data <- data[-10]


# Try with 3 classes: 
data$RateofChange <- NA

index <- c(1:145)
for (i in index){
  if (data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 0.33))
  {
    data$RateofChange[i]<- 1
  }
  if (data$HousePrice.RateofChange[i] > quantile(data$HousePrice.RateofChange, 0.33) &
      data$HousePrice.RateofChange[i] <= quantile(data$HousePrice.RateofChange, 0.66))
  {
    data$RateofChange[i]<- 2
  }
  if (data$HousePrice.RateofChange[i] > quantile(data$HousePrice.RateofChange, 0.66))
  {
    data$RateofChange[i]<- 3
  }
}
data <- data[-10] 
  
# Try with two classes: 
index <- c(1:145)
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

data <- data[-10]

data$RateofChange <- as.factor(data$RateofChange) 
set.seed(100)
train <- sample(nrow(data), 0.75*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]


# Create a Random Forest model with default parameters
model1 <- randomForest(RateofChange ~ Overall.rank + Score +GDP.per.capita +
                         Healthy.life.expectancy + Freedom.to.make.life.choices +
                         Generosity + Perceptions.of.corruption + HousePrice.AvgIndex.oneYearago +
                         HousePrice.AvgIndex.twoYearsago + Job.Vacancy + 
                         EmploymentRate + longterm_interestrate, data = TrainSet, importance = TRUE)
model1

predTrain <- predict(model1, TrainSet[1:14], type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$RateofChange)  

library(caret)
# Create features and target
data <- na.roughfix(data)
X <- data %>% 
  select(Overall.rank,  GDP.per.capita, #Score,
           Healthy.life.expectancy, Freedom.to.make.life.choices,
           Generosity, Perceptions.of.corruption,  EmploymentRate,Job.Vacancy, #longterm_interestrate,
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
regr <- randomForest(x = X_train, y = y_train, maxnodes = 100, ntree = 100, replace = TRUE)

# Make prediction
predictions <- predict(regr, X_test)

#result <- X_test
#result['prediction'] <- y_test
#result['prediction']<-  predictions
#head(result)

table(predictions,y_test) 
confusionMatrix(predictions,y_test)
regr$importance
