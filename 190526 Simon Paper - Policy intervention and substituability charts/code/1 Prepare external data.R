library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")
library("WDI")

rm(list=ls())

gta_setwd()

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")


#---------------------------------------------#
#                                             #
#   WORLD BANK DATA                           #
#                                             #
#---------------------------------------------#


#---------------------------------------------#
#                                             #
#   Final government consumption spending     #
#                                             #
#---------------------------------------------#

# source: https://databank.worldbank.org/data/reports.aspx?source=2&series=NE.CON.GOVT.KN
# gov.spending <- read.csv(paste0(resources.path,"World Bank - Government Financial Consumption Expenditure 2008-2018.csv"), sep=",", stringsAsFactors = F)
# 
# gov.spending[,c("X...Series.Name", "Series.Code")] <- NULL
# names(gov.spending) <- c("name","ISO",2007:2018)
gov.spending=WDI(indicator="NE.CON.GOVT.KN", start=2007, end=2018, extra=T)
gov.spending=subset(gov.spending, is.na(NE.CON.GOVT.KN)==F & year %in% years.needed & iso3c %in% country.names$iso_code)

gov.spending2=WDI(indicator="NE.CON.GOVT.KD", start=2007, end=2018, extra=T)
gov.spending2=subset(gov.spending2, is.na(NE.CON.GOVT.KD)==F & year %in% years.needed & iso3c %in% country.names$iso_code)

cty.presence=aggregate(year ~ iso3c, gov.spending, function(x) length(unique(x)))
cty.presence2=aggregate(year ~ iso3c, gov.spending2, function(x) length(unique(x)))

# missing g20 members
country.names$iso_code[country.names$is.g20==T][! country.names$iso_code[country.names$is.g20==T] %in% cty.presence$iso3c[cty.presence$year==length(years.needed)]]
country.names$iso_code[country.names$is.g20==T][! country.names$iso_code[country.names$is.g20==T] %in% cty.presence2$iso3c[cty.presence2$year==length(years.needed)]]

## Please do the same check for:
# 1) number of next 20 in the set
# 2) number of countries with full coverage
# choose whatever data set is best. If they are the same, use .KN as requested by SE.



# ADD UN CODES AND GTA NAMES
setnames(gov.spending, "iso3c","iso_code")
gov.spending <- merge(gov.spending, country.names[,c("un_code", "iso_code")], by="iso_code")
gov.spending[,c("country","iso_code")] <- NULL

gov.spending <- merge(gov.spending, all, by="un_code")

# RESHAPE
setnames(gov.spending, "NE.CON.GOVT.KN","value")
gov.spending=gov.spending[,c("name","un_code","year","value")]

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
