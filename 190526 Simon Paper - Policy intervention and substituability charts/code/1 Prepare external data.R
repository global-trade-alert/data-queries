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

# missing nextg20 members
country.names$iso_code[country.names$un_code %in% nextg20][! country.names$iso_code[country.names$un_code %in% nextg20] %in% cty.presence$iso3c[cty.presence$year==length(years.needed)]]
country.names$iso_code[country.names$un_code %in% nextg20][! country.names$iso_code[country.names$un_code %in% nextg20] %in% cty.presence2$iso3c[cty.presence2$year==length(years.needed)]]
# number of countries with full coverage
length(cty.presence$iso3c[cty.presence$year == length(years.needed)])
length(cty.presence2$iso3c[cty.presence2$year == length(years.needed)])

# choose whatever data set is best. If they are the same, use .KN as requested by SE.
# --> Both are exactly the same.

## restricting to complete set
gov.spending=subset(gov.spending, iso3c %in% subset(cty.presence, year==4)$iso3c)

# ADD UN CODES AND GTA NAMES
setnames(gov.spending, "iso3c","iso_code")
gov.spending <- merge(gov.spending, country.names[,c("un_code", "iso_code")], by="iso_code")
gov.spending[,c("country","iso_code")] <- NULL

gov.spending <- merge(gov.spending, all, by="un_code")

# RESHAPE
setnames(gov.spending, "NE.CON.GOVT.KN","value")
gov.spending=gov.spending[,c("name","un_code","year","value")]

# CALCULATE GROWTH RATES
# SE wants a scatter plot with one dot per country. 
# Also, the growth rates have to be calculated for each period separtely.
gov.spending.result=data.frame()

for(i in unique(gov.spending$un_code)){
  for(prd in 1:3){
    yr.start=year(periods[[prd]])[1]
    yr.end=year(periods[[prd]])[2]
    
    gr=subset(gov.spending, un_code==i & year==yr.end)$value/subset(gov.spending, un_code==i & year==yr.start)$value -1
    
    gov.spending.result=rbind(gov.spending.result,
                              data.frame(un_code=i,
                                         name=country.names$name[country.names$un_code==i],
                                         period.id=prd,
                                         period.start=yr.start,
                                         period.end=yr.end,
                                         growth.rate=gr,
                                         stringsAsFactors = F))
    
    rm(gr)
    
  }
  print(i)
}

#---------------------------------------------#
#                                             #
#   Bilateral exchange rate                   #
#                                             #
#---------------------------------------------#

# source: https://databank.worldbank.org/data/reports.aspx?source=1179&series=DPANUSSPB
exch.rate <- WDI(indicator="DPANUSSPB", start=2007, end=2018, extra=T)
exch.rate=subset(exch.rate, is.na(DPANUSSPB)==F & year %in% years.needed & iso3c %in% country.names$iso_code)

## restricting to complete set
cty.presence=aggregate(year ~ iso3c, exch.rate, function(x) length(unique(x)))
exch.rate=subset(exch.rate, iso3c %in% subset(cty.presence, year==4)$iso3c)

# ADD UN CODES AND GTA NAMES
setnames(exch.rate, "iso3c","iso_code")
exch.rate <- merge(exch.rate, country.names[,c("un_code", "iso_code")], by="iso_code")
exch.rate[,c("country","iso_code")] <- NULL

exch.rate <- merge(exch.rate, all, by="un_code")

# RESHAPE
setnames(exch.rate, "DPANUSSPB","value")
exch.rate=exch.rate[,c("name","un_code","year","value")]

exch.rate.result=data.frame()

for(i in unique(exch.rate$un_code)){
  for(prd in 1:3){
    yr.start=year(periods[[prd]])[1]
    yr.end=year(periods[[prd]])[2]
    
    gr=subset(exch.rate, un_code==i & year==yr.end)$value/subset(exch.rate, un_code==i & year==yr.start)$value -1
    
    exch.rate.result=rbind(exch.rate.result,
                              data.frame(un_code=i,
                                         name=country.names$name[country.names$un_code==i],
                                         period.id=prd,
                                         period.start=yr.start,
                                         period.end=yr.end,
                                         growth.rate=gr,
                                         stringsAsFactors = F))
    
    rm(gr)
    
  }
  print(i)
}

#---------------------------------------------#
#                                             #
#   Unemployment rate                         #
#                                             #
#---------------------------------------------#

# source: https://databank.worldbank.org/data/reports.aspx?source=1179&series=UNEMPSA_
un.rate <- WDI(indicator="UNEMPSA_", start=2007, end=2018, extra=T)
un.rate=subset(un.rate, is.na(UNEMPSA_)==F & year %in% years.needed & iso3c %in% country.names$iso_code)

## restricting to complete set
cty.presence=aggregate(year ~ iso3c, un.rate, function(x) length(unique(x)))
un.rate=subset(un.rate, iso3c %in% subset(cty.presence, year==4)$iso3c)

# ADD UN CODES AND GTA NAMES
setnames(un.rate, "iso3c","iso_code")
un.rate <- merge(un.rate, country.names[,c("un_code", "iso_code")], by="iso_code")
un.rate[,c("country","iso_code")] <- NULL

un.rate <- merge(un.rate, all, by="un_code")

# RESHAPE
setnames(un.rate, "UNEMPSA_","value")
un.rate=un.rate[,c("name","un_code","year","value")]


un.rate.result=data.frame()

for(i in unique(un.rate$un_code)){
  for(prd in 1:3){
    yr.start=year(periods[[prd]])[1]
    yr.end=year(periods[[prd]])[2]
    
    gr=subset(un.rate, un_code==i & year==yr.end)$value/subset(un.rate, un_code==i & year==yr.start)$value -1
    
    un.rate.result=rbind(un.rate.result,
                           data.frame(un_code=i,
                                      name=country.names$name[country.names$un_code==i],
                                      period.id=prd,
                                      period.start=yr.start,
                                      period.end=yr.end,
                                      growth.rate=gr,
                                      stringsAsFactors = F))
    
    rm(gr)
    
  }
  print(i)
}


# SAVE ALL DATAFRAMES
gov.spending <- gov.spending.result
exch.rate <- exch.rate.result
un.rate <- un.rate.result
save(gov.spending, exch.rate, un.rate, file=paste0(data.path, "worldbankdata.Rdata"))
