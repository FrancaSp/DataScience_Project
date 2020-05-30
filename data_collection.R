## Einbindung der Datensätze 
library(tidyverse)
library(gdata)
#jobs <- read.table(file='~/Google Drive/Data Science Abgabe/Daten/jobs.tsv',sep = '\t', header = TRUE)
jobs <- read.csv('~/Google Drive/Data Science Abgabe/Daten/jobs.csv')
interestrates <- read.csv('~/Google Drive/Data Science Abgabe/Daten/intrestrates.csv')
houseprices <- read.csv('~/Google Drive/Data Science Abgabe/Daten/housingprices.csv')
happy15 <- read.csv('~/Google Drive/Data Science Abgabe/Daten/happy2015.csv')
happy16 <- read.csv('~/Google Drive/Data Science Abgabe/Daten/happy2016.csv')
happy17 <- read.csv('~/Google Drive/Data Science Abgabe/Daten/happy2017.csv')
happy18 <- read.csv('~/Google Drive/Data Science Abgabe/Daten/happy2018.csv')
happy19 <- read.csv('~/Google Drive/Data Science Abgabe/Daten/happy2019.csv')

#missing values
nrow(filter(filter(interestrates, INT_RT == "12-month rate"),Value == ":"))
nrow(filter(filter(interestrates, INT_RT == "3-month rate"),Value == ":"))
nrow(filter(filter(interestrates, INT_RT == "1-month rate"),Value == ":"))
nrow(filter(filter(interestrates, INT_RT == "6-month rate"),Value == ":"))
nrow(filter(filter(interestrates, INT_RT == "Day-to-day rate"),Value == ":"))
## since 3-month rate has the lowest level of missing values this value will be prcessed futher 

threemonth_interestrates <- filter(interestrates, INT_RT == "3-month rate")

houseprice_total <- filter(houseprices, PURCHASE== "Total" & TIME > 2014) 
View(houseprice_total)

happy15 <- happy15 %>% rename(Score = Happiness.Score, Overall.rank = Happiness.Rank, GDP.per.capita = Economy..GDP.per.Capita.,
                              Healthy.life.expectancy = Health..Life.Expectancy., Freedom.to.make.life.choices = Freedom,
                              Perceptions.of.corruption = Trust..Government.Corruption.)

happy16 <- happy16 %>% rename(Score = Happiness.Score, Overall.rank = Happiness.Rank, GDP.per.capita = Economy..GDP.per.Capita.,
                   Healthy.life.expectancy = Health..Life.Expectancy., Freedom.to.make.life.choices = Freedom,
                   Perceptions.of.corruption = Trust..Government.Corruption.)

happy17 <- happy17 %>% rename(Score = Happiness.Score, Overall.rank = Happiness.Rank, GDP.per.capita = Economy..GDP.per.Capita.,
                   Healthy.life.expectancy = Health..Life.Expectancy., Freedom.to.make.life.choices = Freedom,
                   Perceptions.of.corruption = Trust..Government.Corruption.)

happy18 <- happy18 %>% rename( Country = Country.or.region)

happy19 <- happy19 %>% rename( Country = Country.or.region)

# delete social.support variable because it is not defined further and cant be found in data fram 15, 16 and 17 

happy19 <- happy19[-5]
happy18 <- happy18[-5]

happy15 = subset(happy15, select = c(colnames(happy19)))
happy16 = subset(happy16, select = c(colnames(happy19)))
happy17 = subset(happy17, select = c(colnames(happy19)))

View(filter(houseprice_total, TIME == 2015 & UNIT == "Annual average rate of change"))

threemonth_interestrates <- filter(threemonth_interestrates, TIME > 2014) 

#convert factor var inn numeric
houseprice_total$Value <- as.numeric(as.character(houseprice_total$Value))
houseprice_total$GEO <- as.character(houseprice_total$GEO)
happy16$HousePrice.RateofChange <- NA
                                     
# index <- c(1:30)
# for (i in index) {
#   if (tmp$GEO[i] == "Germany (until 1990 former territory of the FRG)")
#   {happy16$Houseprice.Rateofchange[16] <- as.numeric(as.character(tmp$Value[i]))}
#   else i = 40
# }

index <- c(1:350)
for (i in index) {
  if (houseprice_total$GEO[i] == "Germany (until 1990 former territory of the FRG)")
  {houseprice_total$GEO[i] <- "Germany"}
  else i = 400
}


extractinfo<-function(
  dfone,
  dftwo,
  comparisonvar,
  comparisonvartwo,
  goalvar,
  valueofgoal
)
{
  for(x in c(1:nrow(dfone))) {
    for (y in c(1:nrow(dftwo))) {
      if(dfone[x, comparisonvar] == dftwo[y, comparisonvartwo])
      {
        dftwo[y, goalvar] <- dfone[x, valueofgoal]
      }
    }
  }
  return(dftwo)
}
# include housing pricesin happiness datasets
tmp <- filter(houseprice_total, TIME == 2016 & UNIT == "Annual average rate of change") %>% slice(6:35)
happy16 <- extractinfo(tmp, happy16, "GEO", "Country", "HousePrice.RateofChange", "Value")

tmp <- filter(houseprice_total, TIME == 2016 & UNIT == "Annual average index") %>% slice(6:35)
happy16$HousePrice.AvgIndex <- NA 
happy16<-extractinfo(tmp, happy16, "GEO", "Country", "HousePrice.AvgIndex", "Value")

tmp <- filter(houseprice_total, TIME == 2015 & UNIT == "Annual average rate of change") %>% slice(6:35)
happy15$HousePrice.RateofChange <- NA 
happy15 <- extractinfo(tmp, happy15, "GEO", "Country", "HousePrice.RateofChange", "Value")

tmp <- filter(houseprice_total, TIME == 2015 & UNIT == "Annual average index") %>% slice(6:35)
happy15$HousePrice.AvgIndex <- NA 
happy15 <- extractinfo(tmp, happy15, "GEO", "Country", "HousePrice.AvgIndex", "Value")

tmp <- filter(houseprice_total, TIME == 2017 & UNIT == "Annual average rate of change") %>% slice(6:35)
happy17$HousePrice.RateofChange <- NA 
happy17 <- extractinfo(tmp, happy17, "GEO", "Country", "HousePrice.RateofChange", "Value")

tmp <- filter(houseprice_total, TIME == 2017 & UNIT == "Annual average index") %>% slice(6:35)
happy17$HousePrice.AvgIndex <- NA 
happy17 <- extractinfo(tmp, happy17, "GEO", "Country", "HousePrice.AvgIndex", "Value")

tmp <- filter(houseprice_total, TIME == 2018 & UNIT == "Annual average rate of change") %>% slice(6:35)
happy18$HousePrice.RateofChange <- NA 
happy18 <- extractinfo(tmp, happy18, "GEO", "Country", "HousePrice.RateofChange", "Value")

tmp <- filter(houseprice_total, TIME == 2018 & UNIT == "Annual average index") %>% slice(6:35)
happy18$HousePrice.AvgIndex <- NA 
happy18 <- extractinfo(tmp, happy18, "GEO", "Country", "HousePrice.AvgIndex", "Value")

tmp <- filter(houseprice_total, TIME == 2019 & UNIT == "Annual average rate of change") %>% slice(6:35)
happy19$HousePrice.RateofChange <- NA 
happy19 <- extractinfo(tmp, happy19, "GEO", "Country", "HousePrice.RateofChange", "Value")

tmp <- filter(houseprice_total, TIME == 2019 & UNIT == "Annual average index") %>% slice(6:35)
happy19$HousePrice.AvgIndex <- NA 
happy19 <- extractinfo(tmp, happy19, "GEO", "Country", "HousePrice.AvgIndex", "Value")


## collect interest rates and include tehm in happyness datasets 
threemonth_interestrates$Value <- as.numeric(as.character(threemonth_interestrates$Value))
threemonth_interestrates$GEO <- as.character(threemonth_interestrates$GEO)
tmp <- filter(threemonth_interestrates, TIME == 2015) 
happy15$Threemonth.InterestRates <- NA 
happy15 <- extractinfo(tmp, happy15, "GEO", "Country", "Threemonth.InterestRates", "Value")

tmp <- filter(threemonth_interestrates, TIME == 2016) 
happy16$Threemonth.InterestRates <- NA 
happy16 <- extractinfo(tmp, happy16, "GEO", "Country", "Threemonth.InterestRates", "Value")

tmp <- filter(threemonth_interestrates, TIME == 2017) 
happy17$Threemonth.InterestRates <- NA 
happy17 <- extractinfo(tmp, happy17, "GEO", "Country", "Threemonth.InterestRates", "Value")

tmp <- filter(threemonth_interestrates, TIME == 2018) 
happy18$Threemonth.InterestRates <- NA 
happy18 <- extractinfo(tmp, happy18, "GEO", "Country", "Threemonth.InterestRates", "Value")

tmp <- filter(threemonth_interestrates, TIME == 2019) 
happy19$Threemonth.InterestRates <- NA 
happy19 <- extractinfo(tmp, happy19, "GEO", "Country", "Threemonth.InterestRates", "Value")

## collect job vacacy information and include it in happiness data 
jobs$Value <- as.numeric(as.character(jobs$Value))
jobs$GEO <- as.character(jobs$GEO)
index <- c(1:370)
for (i in index) {
  if (jobs$GEO[i] == "Germany (until 1990 former territory of the FRG)")
  {jobs$GEO[i] <- "Germany"}
}

tmp <- filter(jobs, TIME == 2015) 
happy15$Job.Vacancy <- NA 
happy15 <- extractinfo(tmp, happy15, "GEO", "Country", "Job.Vacancy", "Value")

tmp <- filter(jobs, TIME == 2016) 
happy16$Job.Vacancy <- NA 
happy16 <- extractinfo(tmp, happy16, "GEO", "Country", "Job.Vacancy", "Value")

tmp <- filter(jobs, TIME == 2017) 
happy17$Job.Vacancy <- NA 
happy17 <- extractinfo(tmp, happy17, "GEO", "Country", "Job.Vacancy", "Value")

tmp <- filter(jobs, TIME == 2018) 
happy18$Job.Vacancy <- NA 
happy18 <- extractinfo(tmp, happy18, "GEO", "Country", "Job.Vacancy", "Value")

tmp <- filter(jobs, TIME == 2019) 
happy19$Job.Vacancy <- NA 
happy19 <- extractinfo(tmp, happy19, "GEO", "Country", "Job.Vacancy", "Value")



# exclude the missing values of non european countries 

happy15 <- filter(happy15,happy15$HousePrice.AvgIndex != 'NA')
happy16 <- filter(happy16,happy16$HousePrice.AvgIndex != 'NA')
happy16 <- filter(happy16,happy16$HousePrice.AvgIndex != 'NA')
happy17 <- filter(happy17,happy17$HousePrice.AvgIndex != 'NA')
happy18 <- filter(happy18,happy18$HousePrice.AvgIndex != 'NA')
happy19 <- filter(happy19,happy19$HousePrice.AvgIndex != 'NA')

employmentrate <- read.xls("~/Google Drive/Data Science Abgabe/Daten/Erwerbstaetigenquote.xls")
employmentrate$Land
country_names <- c("Belgium", 'Denmark', 'Germany', 'Estonia', 'Finland', 'France', 'Greece', 
                   'Ireland','Iceland','Italy', 'Luxembourg', 'Netherlands','Norway', 'Austria', 'Poland',
                   'Portugal', 'Sweden', 'Switzerland', 'Slovakia', 'Slovenia', 'Spain', 'Czech Republic',
                   'Turkey', 'Hungary', 'United Kingdom')

employmentrate$Land <- country_names

happy15$EmploymentRate <- NA 
happy15 <- extractinfo(employmentrate, happy15, "Land", "Country", "EmploymentRate", "X2015")

happy16$EmploymentRate <- NA 
happy16 <- extractinfo(employmentrate, happy16, "Land", "Country", "EmploymentRate", "X2016")

happy17$EmploymentRate <- NA 
happy17 <- extractinfo(employmentrate, happy17, "Land", "Country", "EmploymentRate", "X2017")

happy18$EmploymentRate <- NA 
happy18 <- extractinfo(employmentrate, happy18, "Land", "Country", "EmploymentRate", "X2018")

BIP <- read.xls("~/Google Drive/Data Science Abgabe/Daten/Bruttoinlandsprodukt_je_Einwohner.xls")
BIP$Land
country_names <- c("Belgium", 'Bulgaria', 'Denmark', 'Germany', 'Estonia', 'Finland', 'France', 'Greece', 
                   'Ireland','Iceland','Italy', 'Croatia','Latvia','Lithuania', 'Luxembourg', 'Malta',
                   'Netherlands','Norway', 'Austria', 'Poland',
                   'Portugal','Romania', 'Sweden', 'Switzerland', 'Slovakia', 'Slovenia', 'Spain', 'Czech Republic',
                   'Hungary', 'United Kingdom', 'Cyprus')
BIP$Land<- country_names

happy15$BIP <- NA 
happy15 <- extractinfo(BIP, happy15, "Land", "Country", "BIP", "X2015")

happy16$BIP <- NA 
happy16 <- extractinfo(BIP, happy16, "Land", "Country", "BIP", "X2016")

happy17$BIP <- NA 
happy17 <- extractinfo(BIP, happy17, "Land", "Country", "BIP", "X2017")

happy18$BIP <- NA 
happy18 <- extractinfo(BIP, happy18, "Land", "Country", "BIP", "X2018")

interestrates_longterm <- read.xls("~/Google Drive/Data Science Abgabe/Daten/Zinsen_langfristig.xls")

happy15$longterm_interestrates <- NA 
happy15 <- extractinfo(interestrates_longterm, happy15, "Land", "Country", "longterm_interestrates", "X2015")

happy16$longterm_interestrate <- NA 
happy16 <- extractinfo(interestrates_longterm, happy16, "Land", "Country", "longterm_interestrate", "X2016")

happy17$longterm_interestrate <- NA 
happy17 <- extractinfo(interestrates_longterm, happy17, "Land", "Country", "longterm_interestrate", "X2017")

happy18$longterm_interestrate <- NA 
happy18 <- extractinfo(interestrates_longterm, happy18, "Land", "Country", "longterm_interestrate", "X2018")

