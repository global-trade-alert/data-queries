library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")

rm(list=ls())

setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")


#---------------------------------------------#
#                                             #
#   WORLD BANK DATA                           #
#                                             #
#---------------------------------------------#

# CLEAN COUNTRIES FILE
countries <- read.csv("R help files/country_iso_un.csv",sep=";", stringsAsFactors = F)

# USED TO ADD UN CODES TO WORLD BANK DATASET
countries$ISO[countries$gta.name == "Monaco"] <- "MCO"
countries$ISO[countries$gta.name == "Micronesia"] <- "FSM"
countries$ISO[countries$gta.name == "Antigua & Barbuda"] <- "ATG"
countries$ISO[countries$gta.name == "Faeroe Islands"] <- "FRO"
countries$ISO[countries$gta.name == "Grenada"] <- "GRD"
countries$ISO[countries$gta.name == "Greenland"] <- "GRL"
countries$ISO[countries$gta.name == "Liechtenstein"] <- "LIE"
countries$ISO[countries$gta.name == "Macao"] <- "MAC"
countries$ISO[countries$gta.name == "Saint-Martin"] <- "MAF"
countries$ISO[countries$gta.name == "Marshall Islands"] <- "MHL"
countries$ISO[countries$gta.name == "Northern Mariana Islands"] <- "MNP"
countries$ISO[countries$gta.name == "Nauru"] <- "NRU"
countries$ISO[countries$gta.name == "Palau"] <- "PLW"
countries$ISO[countries$gta.name == "French Polynesia"] <- "PYF"
countries$ISO[countries$gta.name == "San Marino"] <- "SMR"
countries$ISO[countries$gta.name == "Turks & Caicos Islands"] <- "TCA"
countries$ISO[countries$gta.name == "Timor-Leste"] <- "TLS"


#---------------------------------------------#
#                                             #
#   Final government consumption spending     #
#                                             #
#---------------------------------------------#

# source: https://databank.worldbank.org/data/reports.aspx?source=2&series=NE.CON.GOVT.KN
gov.spending <- read.csv(paste0(resources.path,"World Bank - Government Financial Consumption Expenditure 2008-2018.csv"), sep=",", stringsAsFactors = F)

gov.spending[,c("X...Series.Name", "Series.Code")] <- NULL
names(gov.spending) <- c("name","ISO",2007:2018)

# ADD UN CODES AND GTA NAMES
gov.spending <- merge(gov.spending, countries[,c("UN", "ISO")], by="ISO")
gov.spending[,c("name","ISO")] <- NULL
setnames(gov.spending, "UN","un_code")
gov.spending <- merge(gov.spending, all, by="un_code")

# RESHAPE
gov.spending <- gov.spending[,c("name","un_code",paste0(2007:2018))]
gov.spending <- gather(gov.spending, year, value, 3:ncol(gov.spending))

# CALCULATE GROWTH RATES
gov.spending$value <- as.numeric(gov.spending$value)
gov.spending[is.na(gov.spending)] <- NA

# CALCULATE GROUP VALUES
gov.spending.result <- data.frame(name=character(),
                                  value=numeric(),
                                  year=numeric())

# COMPUTE SUMS PER GROUP AND... 
for (g in 1:length(groups)) {
  
  gov.temp <- aggregate(value ~ year, subset(gov.spending, un_code %in% groups[[g]]), function(x) sum(x))
  
  gov.spending.result <- rbind(gov.spending.result, data.frame(name=groups.name[g],
                                                               value=gov.temp$value,
                                                               year=gov.temp$year))
}

# ... CALCULATE GROWTH VALUES FOR THE SUMS
gov.spending.result$growth <- with(gov.spending.result, ave(value, name, 
                                      FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))

gov.spending.result$year <- as.numeric(as.character(gov.spending.result$year))
gov.spending.result <- subset(gov.spending.result, year > 2007 & year <= 2016)

#---------------------------------------------#
#                                             #
#   Bilateral exchange rate                   #
#                                             #
#---------------------------------------------#

# source: https://databank.worldbank.org/data/reports.aspx?source=1179&series=DPANUSSPB
exch.rate <- read.csv(paste0(resources.path,"World Bank - Exchange Rates 2007-2018.csv"), sep=",", stringsAsFactors = F)
exch.rate[,c("X...Series","Series.Code")] <- NULL
exch.rate <- exch.rate[,c(1,2,seq(3,153,13))]

names(exch.rate) <- c("name","ISO",2007:2018)

# ADD UN CODES AND GTA NAMES
exch.rate <- merge(exch.rate, countries[,c("UN", "ISO")], by="ISO")
exch.rate[,c("name","ISO")] <- NULL
setnames(exch.rate, "UN","un_code")
exch.rate <- merge(exch.rate, all, by="un_code")

# RESHAPE
exch.rate <- exch.rate[,c("name","un_code",paste0(2007:2018))]
exch.rate <- gather(exch.rate, year, value, 3:ncol(exch.rate))

# CALCULATE GROWTH RATES TO BE ABLE TO MAKE STATEMENTS ON GROUP EXCHANGE RATES
exch.rate$value <- as.numeric(exch.rate$value)
exch.rate$growth <- with(exch.rate, ave(value, name, 
                                        FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
exch.rate <- subset(exch.rate, year > 2007 & year <= 2016)

# ! VENEZUELA HAS A EXCHANGE RATE DEVALUATION OF 45500 % in 2018, I WILL THEREFORE REMOVE THIS VALUE
# ALTHOUGH 2018 DATA IS UNUSED ANYWAY SO...
exch.rate$growth[exch.rate$name == "Venezuela" & exch.rate$year == 2018] <- NA

# CALCULATE GROUP VALUES
exch.rate.result <- data.frame(name=character(),
                               value=numeric(),
                               year=numeric())

for (g in 1:length(groups)) {
  exch.temp <- aggregate(growth ~ year, subset(exch.rate, un_code %in% groups[[g]]), function(x) mean(x))
  exch.rate.result <- rbind(exch.rate.result, data.frame(name=groups.name[g],
                                                         value=exch.temp$growth,
                                                         year=exch.temp$year))
}


#---------------------------------------------#
#                                             #
#   Unemployment rate                         #
#                                             #
#---------------------------------------------#

# source: https://databank.worldbank.org/data/reports.aspx?source=1179&series=UNEMPSA_
un.rate <- read.csv(paste0(resources.path,"World Bank - Unemployment Rate 2007-2018.csv"), sep=",", stringsAsFactors = F)
un.rate[,c("X...Series","Series.Code")] <- NULL
un.rate <- un.rate[,c(1,2,seq(3,153,13))]

names(un.rate) <- c("name","ISO",2007:2018)

# ADD UN CODES AND GTA NAMES
un.rate <- merge(un.rate, countries[,c("UN", "ISO")], by="ISO")
un.rate[,c("name","ISO")] <- NULL
setnames(un.rate, "UN","un_code")
un.rate <- merge(un.rate, all, by="un_code")

# RESHAPE
un.rate <- un.rate[,c("name","un_code",paste0(2007:2018))]
un.rate <- gather(un.rate, year, value, 3:ncol(un.rate))
un.rate <- subset(un.rate, year > 2007 & year <= 2016)

# CALCULATE GROWTH RATES TO BE ABLE TO MAKE STATEMENTS ON GROUP EXCHANGE RATES
un.rate$value <- as.numeric(un.rate$value)
un.rate$growth <- with(un.rate, ave(value, name, 
                                        FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
un.rate <- subset(un.rate, year > 2007 & year <= 2016)

#### ! QATAR HAS A UNEMPLOYMENT RATES OF ~251000 % IN ALL YEARS, I WILL THEREFORE REMOVE THIS VALUE
un.rate$value[un.rate$name == "Qatar"] <- NA

# CALCULATE GROUP VALUES
un.rate.result <- data.frame(name=character(),
                             value=numeric(),
                             year=numeric())

for (g in 1:length(groups)) {
  un.temp <- aggregate(growth ~ year, subset(un.rate, un_code %in% groups[[g]]), function(x) mean(x))
  un.rate.result <- rbind(un.rate.result, data.frame(name=groups.name[g],
                                                     value=un.temp$growth,
                                                     year=un.temp$year))
}


# SAVE ALL DATAFRAMES
gov.spending <- gov.spending.result
exch.rate <- exch.rate.result
un.rate <- un.rate.result
save(gov.spending, exch.rate, un.rate, file=paste0(data.path, "worldbankdata.Rdata"))
