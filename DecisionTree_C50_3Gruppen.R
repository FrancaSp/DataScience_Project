##### Decision Tree - (C5.0 Algorithmus) - 3 Gruppen #####


library(readr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ipred)
library(C50)


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



### (3) Datensatz in Training und Test aufteilen (%-Satz Training):
set.seed(12)

split_id <- sample(1:nrow(daten), size = floor(0.7 * nrow(daten)), replace=FALSE) 
daten_train <- daten[split_id,]
daten_test <- daten[-split_id,]



### (4) C5.0 Classification Tree
#model <- RateofChange ~ Score + Overall.rank + GDP.per.capita + Healthy.life.expectancy + 
#  Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Job.Vacancy + 
#  EmploymentRate + longterm_interestrate + HousePrice.AvgIndex.oneYearago + HousePrice.AvgIndex.twoYearsago
model <- RateofChange ~ Score + GDP.per.capita + Healthy.life.expectancy + 
  Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Job.Vacancy + 
  EmploymentRate + longterm_interestrate + HousePrice.AvgIndex.oneYearago + HousePrice.AvgIndex.twoYearsago
#+ Overall.rank

tree_C50 <- C5.0(x=daten_train, y=as.factor(daten_train$RateofChange))
tree_C50 <- C5.0(model, data=daten_train, trials=30)

print(tree_C50)
plot(tree_C50)



### (5) Vorhersagen von Testdatensatz:
pred <- predict(tree_C50, daten_test)
#daten_test$C50_pred <- pred


### (6) Vergleich Vorhersagen & Tatsächliche Gruppe von Testdatensazt mittels Confusion Matrix:
daten_test$RateofChange <- as.factor(daten_test$RateofChange)
confusionMatrix(pred, as.factor(daten_test$RateofChange), dnn = c("Prediction", "Actual Data"))



