#Module One Technique Practice
#Group 1
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(moments)
library(scales)
library(glue)

#Import the CSV file
DS <- read.csv("owid-covid-data.csv", header = TRUE, sep = ",")

#What did you do with the data in the context of exploration? What is the use-case?
DS$date <- as.Date(DS$date, format = "%Y-%m-%d")
Covid <- DS[DS$date > "2021-01-01" & DS$date < "2022-07-01",]
USA <- filter(Covid, Covid$location == "United States")
CAN <- filter(Covid, Covid$location == "Canada")
USAdf <-
  data.frame(
    USA$location,
    USA$date,
    USA$population,
    USA$total_cases_per_million,
    USA$new_deaths_per_million,
    USA$total_deaths_per_million,
    USA$new_cases_per_million,
    USA$reproduction_rate,
    USA$icu_patients_per_million,
    USA$positive_rate,
    USA$people_fully_vaccinated_per_hundred,
    USA$total_boosters,
    USA$stringency_index
  )
USAdf
CANdf <-
  data.frame(
    CAN$location,
    CAN$date,
    CAN$population,
    CAN$total_cases_per_million,
    CAN$new_deaths_per_million,
    CAN$total_deaths_per_million,
    CAN$new_cases_per_million,
    CAN$reproduction_rate,
    CAN$icu_patients_per_million,
    CAN$positive_rate,
    CAN$people_fully_vaccinated_per_hundred,
    CAN$total_boosters,
    CAN$stringency_index
  )
CANdf

#How many entries are in the dataset?
str(USAdf)
str(CANdf)


#Was there missing data? Duplications? How clean was the data? How did you impute missing values?
View(USAdf)
View(CANdf)

which(is.na(USAdf))
USAdf[is.na(USAdf)] = 0
USAdf

CANdf[is.na(CANdf)] = 0
CANdf

#Were there outliers or suspicious data?
par(mfcol = c(3, 2))
plot(
  USAdf$USA.date,
  USAdf$USA.total_cases_per_million,
  ylab = "Total Cases Per Million",
  xlab = "Date",
  main = "Total Cases Per Million in USA"
)
plot(
  USAdf$USA.date,
  USAdf$USA.total_deaths_per_million,
  ylab = "Total Deaths Per Million",
  xlab = "Date",
  main = "Total Deaths Per Million in USA"
)
hist(USA$reproduction_rate, xlab = "reproducation rate", main = "Reproduction rate Frequency distribution histogram")
plot(
  USAdf$USA.date,
  USAdf$USA.positive_rate,
  xlab = "Date",
  ylab = "Positive Rate",
  main = "Positive Rate in USA"
)
plot(
  USAdf$USA.date,
  USAdf$USA.people_fully_vaccinated_per_hundred,
  ylab = "people fully vaccinated",
  xlab = "Date",
  main = "People fully vaccinated in USA"
)
plot(
  USAdf$USA.date,
  USAdf$USA.total_boosters,
  ylab = "total boosters",
  xlab = "Date",
  main = "Total boosters in USA"
)


par(mfcol = c(3, 2))
plot(
  CANdf$CAN.date,
  CANdf$CAN.total_cases_per_million,
  ylab = "Total Cases Per Million",
  xlab = "Date",
  main = "Total Cases Per Million in CAN"
)
plot(
  CANdf$CAN.date,
  CANdf$CAN.total_deaths_per_million,
  ylab = "Total Deaths Per Million",
  xlab = "Date",
  main = "Total Deaths Per Million in CAN"
)
hist(CAN$reproduction_rate, xlab = "reproducation rate", main = "Reproduction rate Frequency distribution histogram")
plot(
  CANdf$CAN.date,
  CANdf$CAN.positive_rate,
  xlab = "Date",
  ylab = "Positive Rate",
  main = "Positive Rate in CAN"
)
plot(
  CANdf$CAN.date,
  CANdf$CAN.people_fully_vaccinated_per_hundred,
  ylab = "people fully vaccinated",
  xlab = "Date",
  main = "People fully vaccinated in CAN"
)
plot(
  CANdf$CAN.date,
  CANdf$CAN.total_boosters,
  ylab = "total boosters",
  xlab = "Date",
  main = "Total boosters in CAN"
)

#One-hot
install.packages("mltools")
library(mltools)
library(data.table)
USAdf$USA.stringency_index[USAdf$USA.stringency_index >= 70] <- 4
USAdf$USA.stringency_index[USAdf$USA.stringency_index >= 60 &
                             USAdf$USA.stringency_index <= 70] <- 3
USAdf$USA.stringency_index[USAdf$USA.stringency_index >= 50 &
                             USAdf$USA.stringency_index <= 60] <- 2
USAdf$USA.stringency_index[USAdf$USA.stringency_index >= 40 &
                             USAdf$USA.stringency_index <= 50] <- 1
USAdf$USA.stringency_index[USAdf$USA.stringency_index >= 30 &
                             USAdf$USA.stringency_index <= 40] <- 0
USAdf$USA.stringency_index <- as.factor(USAdf$USA.stringency_index)
levels(USAdf$USA.stringency_index)
One_Hot_USA_Stringency_index <-
  one_hot(as.data.table(USAdf$USA.stringency_index))
View(One_Hot_USA_Stringency_index)

CANdf$CAN.stringency_index[CANdf$CAN.stringency_index >= 70] <- 4
CANdf$CAN.stringency_index[CANdf$CAN.stringency_index >= 60 &
                             CANdf$CAN.stringency_index <= 70] <- 3
CANdf$CAN.stringency_index[CANdf$CAN.stringency_index >= 50 &
                             CANdf$CAN.stringency_index <= 60] <- 2
CANdf$CAN.stringency_index[CANdf$CAN.stringency_index >= 40 &
                             CANdf$CAN.stringency_index <= 50] <- 1
CANdf$CAN.stringency_index[CANdf$CAN.stringency_index >= 20 &
                             CANdf$CAN.stringency_index <= 30] <- 0
CANdf$CAN.stringency_index <- as.factor(CANdf$CAN.stringency_index)
levels(CANdf$CAN.stringency_index)
One_Hot_CAN_Stringency_index <-
  one_hot(as.data.table(CANdf$CAN.stringency_index))
View(One_Hot_CAN_Stringency_index)

USAdf$USA.probably_new_per_million <-
  (USAdf$USA.total_cases_per_million - USAdf$USA.total_deaths_per_million) *
  USAdf$USA.reproduction_rate / 100

CANdf$CAN.probably_new_per_million <-
  (CANdf$CAN.total_cases_per_million - CANdf$CAN.total_deaths_per_million) *
  CANdf$CAN.reproduction_rate / 100


par(mfcol = c(3, 2))
boxplot(USAdf$USA.total_cases_per_million,
        ylab = "total cases(per million)",
        main = "USA total cases")
boxplot(USAdf$USA.total_deaths_per_million,
        ylab = "total deaths(per million)",
        main = "USA total deaths")
boxplot(USAdf$USA.new_cases_per_million,
        ylab = "new cases(per million)",
        main = "USA new cases")
boxplot(CANdf$CAN.total_cases_per_million,
        ylab = "total cases(per million)",
        main = "CAN total cases")
boxplot(CANdf$CAN.total_deaths_per_million,
        ylab = "total deaths(per million)",
        main = "CAN total deaths")
boxplot(CANdf$CAN.new_cases_per_million,
        ylab = "new cases(per million)",
        main = "CAN new cases")
