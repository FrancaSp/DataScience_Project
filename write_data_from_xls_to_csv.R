library(gdata)
library(utils)

employmentrate <- read.xls("~/Google Drive/Data Science Abgabe/Daten/Erwerbstaetigenquote.xls")
employmentrate$Land
country_names <- c("Belgium", 'Denmark', 'Germany', 'Estonia', 'Finland', 'France', 'Greece', 
                   'Ireland','Iceland','Italy', 'Luxembourg', 'Netherlands','Norway', 'Austria', 'Poland',
                   'Portugal', 'Sweden', 'Switzerland', 'Slovakia', 'Slovenia', 'Spain', 'Czech Republic',
                   'Turkey', 'Hungary', 'United Kingdom')

employmentrate$Land <- country_names


interestrates_longterm <- read.xls("~/Google Drive/Data Science Abgabe/Daten/Zinsen_langfristig.xls")

write.csv(employmentrate,'~/Google Drive/Data Science Abgabe/Daten/Erwerbstaetigenquote.csv' )
write.csv(interestrates_longterm, '~/Google Drive/Data Science Abgabe/Daten/Zinsen_langfristig.csv')
