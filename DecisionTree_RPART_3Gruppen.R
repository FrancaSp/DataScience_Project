##### Decision Tree - (CART Algorithmus) - 3 Gruppen #####


library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ipred)


### (1) Datensatz laden:
all_daten <- read.csv(
  text = getURL('https://raw.githubusercontent.com/FrancaSp/DataScience_Project/master/Datensaetze/finale_daten.csv')
)
daten <- read.csv(
  text = getURL('https://raw.githubusercontent.com/FrancaSp/DataScience_Project/master/Datensaetze/finale_daten.csv')
)

#all_daten <- read_csv("C:/Users/Sonja/Dropbox/Uni/Master Wirtschaftsinformatik - Uni Frankfurt/4. Semester (SoSe 2020)/DS1/Projekt/Datenanalyse/DS1/finale_daten.csv")
#daten <- read_csv("C:/Users/Sonja/Dropbox/Uni/Master Wirtschaftsinformatik - Uni Frankfurt/4. Semester (SoSe 2020)/DS1/Projekt/Datenanalyse/DS1/finale_daten.csv")

daten <- daten[2:ncol(daten)] #Spalte "X1" entfernen



### (2) Abhängige Variable in Gruppen einteilen:
daten$RateofChange <- NA

index <- c(1:145)
for (i in index){
  if (daten$HousePrice.RateofChange[i] <= quantile(daten$HousePrice.RateofChange, 0.33))
  {
    daten$RateofChange[i]<- 1
  }
  if (daten$HousePrice.RateofChange[i] > quantile(daten$HousePrice.RateofChange, 0.33) &
      daten$HousePrice.RateofChange[i] <= quantile(daten$HousePrice.RateofChange, 0.66))
  {
    daten$RateofChange[i]<- 2
  }
  if (daten$HousePrice.RateofChange[i] > quantile(daten$HousePrice.RateofChange, 0.66))
  {
    daten$RateofChange[i]<- 3
  }
}
daten <- daten[-10] 
daten$RateofChange <- as.factor(daten$RateofChange)   



### (3) Datensatz in Training und Test aufteilen (60% Training):
set.seed(12)

train_id <- sample(1:nrow(daten), size = floor(0.7 * nrow(daten)), replace=FALSE) 
daten_train <- daten[train_id,]
daten_test <- daten[-train_id,]



### (4) RPART Classification Tree (data is binary!)
#model <- RateofChange ~ Score + Overall.rank + GDP.per.capita + Healthy.life.expectancy + 
#  Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Job.Vacancy + 
#  EmploymentRate + longterm_interestrate + HousePrice.AvgIndex.oneYearago + HousePrice.AvgIndex.twoYearsago
model <- RateofChange ~ Score + GDP.per.capita + Healthy.life.expectancy + 
  Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Job.Vacancy + 
  EmploymentRate + longterm_interestrate + HousePrice.AvgIndex.oneYearago + HousePrice.AvgIndex.twoYearsago
#+ Overall.rank

tree <- rpart (formula = model , # Dependent variable and independent variables ("." for all variables in the data)
               data = daten_train, # Training data set
               na.action = na.rpart, # What should be done with missings? 
               method = "class", # Decision problem (here: categorical variable, so "classification") 
               parms = list(split = 'gini'), # Gini Index as split criterion
               control = rpart.control(minsplit = 10, # Minimal number of obs. in a node before a split is attempted
                                       cp = 0.01, # Complexity param.: min. increase in split criterion to grow the tree 
                                       #usesurrogate = 2, # How many surrogates are used
                                       maxdepth = 8)) #max. tree depth


#rpart.plot(tree)
rpart.plot(tree,extra=101)
summary(tree)
printcp(tree)
tree$variable.importance



### (5) Vorhersagen von Testdatensatz:
pred <- predict(tree, newdata = daten_test, type="class")
daten_test_2 <- cbind(daten_test, pred)


### (6) Vergleich Vorhersagen & Tatsächliche Gruppe von Testdatensazt mittels Confusion Matrix ####
daten_test_2$RateofChange <- as.factor(daten_test_2$RateofChange)
confusionMatrix(daten_test_2$pred, daten_test_2$RateofChange, 
                dnn = c("Prediction", "Actual Data"))


