library("gtalibrary")
library("xlsx")
library("tidyverse")
library("WDI")
library("IMFData")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")


# GET TOP MARKETS AND SECTORS
load(paste0(data.path,"Top CPC and markets.Rdata"))
top.cpc <- top.cpc$cpc
top.markets <- top.markets$un_code


# TABLE 2
# The second calculations relate only to the top 10 foreign markets for Swiss 
# exports (excluding gold again.) Please calculate the % change in the value 
# of the Swiss franc against the local currency of each top 10 foreign market 
# between 2010 and 2017. Please also calculate for each top 10 foreign market 
# the % change in their real government consumption expenditure between 2010 and 2017.

# EXCHANGE RATE

year.start = 2010
year.end = 2017

# IMF currency values

## Add your currencies here
# imf.cur=data.frame(currency=c("GBP", "PLN", "EUR", "SEK", "DKK", "HUF", "BGN", "CZK", "NOK", "CHF", "HRK", "USD", "RON", "SKK", "MKD", "ISK", "JPY", "LTL", "LVL", "MTL"),
#                    imf.symbol=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"),
#                    stringsAsFactors = F)
# 
# databaseID <- 'IFS'
# startdate='2010-01-01'
# enddate='2017-12-31'
# checkquery = FALSE
# 
# 
# queryfilter <- list(CL_FREQ='A',
#                     CL_AREA_IFS=c("GB", "PL", "U2", "SE", "DK", "HU", "BG","CZ", "NO", "CH", "HR", "US", "RO", "SK","MK", "IS","JP", "LT", "LV", "MT"),
#                     CL_INDICATOR_IFS =c('ENDA_XDC_USD_RATE'))
# ex.rate <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy = T)
# 
# ex.rate=ex.rate[,c(1,2,4)]
# names(ex.rate)=c("year","lcu.per.usd", "imf.symbol")
# ex.rate=merge(ex.rate, imf.cur, by="imf.symbol",all.x=T)
# ex.rate$imf.symbol=NULL
# ex.rate=subset(ex.rate, year %in% c(2010, 2017))
# ex.rate$lcu.per.usd=as.numeric(ex.rate$lcu.per.usd)
# 
# chf.2010=ex.rate$lcu.per.usd[ex.rate$currency=="CHF" & ex.rate$year==2010]
# chf.2017=ex.rate$lcu.per.usd[ex.rate$currency=="CHF" & ex.rate$year==2017]
# 
# ex.rate=reshape(ex.rate, idvar="currency",timevar="year", direction="wide")
# names(ex.rate)=gsub("usd","chf",names(ex.rate))
# 
# ex.rate$lcu.per.chf.2010=ex.rate$lcu.per.chf.2010/chf.2010
# ex.rate$lcu.per.chf.2017=ex.rate$lcu.per.chf.2017/chf.2017
# ex.rate$chf.appreciation=ex.rate$lcu.per.chf.2017/ex.rate$lcu.per.chf.2010-1 ## see EUR: CHF appreciated 24% against EUR 2010->2017

# SAME RESULTS WITH WDI PACKAGE
xchange <- WDI(indicator="DPANUSSPB", start=year.start, end=year.end, extra=T)
ch.xchange <- c(subset(xchange, xchange$iso3c=="CHE" & xchange$year==year.start)$DPANUSSPB,
                subset(xchange, xchange$iso3c=="CHE" & xchange$year==year.end)$DPANUSSPB) # SAVE SWISS RATES

# ADD HONG KONG ISO3
xchange$iso3c[xchange$country == "Hong Kong China"] <- "HKG"

xchange=subset(xchange, is.na(DPANUSSPB)==F & year %in% c(year.start,year.end) & iso3c %in% countries$iso_code[countries$un_code %in% top.markets])[,c("iso3c","DPANUSSPB","year")]
xchange$ch <- ch.xchange[1]
xchange$ch[xchange$year==year.end] <- ch.xchange[2]
xchange$ratio <- xchange$DPANUSSPB/xchange$ch # CALCULATE SWISS EXCHANGE RATE AGAINST LCU
xchange[,c("DPANUSSPB","ch")] <- NULL
xchange <- spread(xchange, year, ratio)
xchange$growth.xchange.rate = ((xchange$`2017`/xchange$`2010`)-1)*100 # CALCULATE GROWTH

# GOV SPENDING, WDI REFERENCE YEAR FOR VALUES IS 2010
realgov <- WDI(indicator="NE.CON.GOVT.CN", start=year.start, end=year.end, extra=T)
realgov <- subset(realgov, is.na(NE.CON.GOVT.CN)==F & year %in% c(year.start,year.end) & iso3c %in% countries$iso_code[countries$un_code %in% top.markets])[,c("iso3c","NE.CON.GOVT.CN","year")]
realgov <- spread(realgov, year, NE.CON.GOVT.CN)

# ADD INFLATION 2010-2017 TO CALCULCATE REAL VALUES OF 2017
cpi.index <- read.csv(paste0(data.path,"consumer price index.csv"))
names(cpi.index)=c("LOCATION", "INDICATOR","SUBJECT","MEASURE","FREQUENCY","TIME","Value" , "Flag.Codes")
cpi.index <- subset(cpi.index, TIME %in% c(year.start,year.end) & LOCATION %in% countries$iso_code[countries$un_code %in% top.markets])[,c("LOCATION","Value","TIME")]
cpi.index <- spread(cpi.index, TIME, Value)
cpi.index$inflation <- cpi.index$`2017`/cpi.index$`2010`
realgov <- merge(realgov,cpi.index[,c("X...LOCATION","inflation")], by.x="iso3c",by.y="X...LOCATION")
realgov$`2017` <- realgov$`2017`/realgov$inflation

realgov$growth.gov.spending <- ((realgov$`2017`/realgov$`2010`)-1)*100 # CALCULATE GROWTH

# ADD SINGAPORE AND HONG KONG GOV SPENDING CHANGE
realother <- WDI(indicator="NE.CON.GOVT.KN", start=year.start, end=year.end, extra=T)
realother <- subset(realother, is.na(NE.CON.GOVT.KN)==F & year %in% c(year.start,year.end) & iso3c %in% c("SGP","HKG"))[,c("iso3c","NE.CON.GOVT.KN","year")]
realother <- spread(realother, year, NE.CON.GOVT.KN)
realother$growth.gov.spending <- ((realother$`2017`/realother$`2010`)-1)*100 # CALCULATE GROWTH

realgov$inflation <- NULL
realgov <- rbind(realgov, realother)

# COMBINE BOTH SETS
table2 <- merge(xchange[c("iso3c","growth.xchange.rate")], realgov[,c("iso3c","growth.gov.spending")], by="iso3c")
table2 <- merge(table2, countries[c("name","iso_code")], by.x="iso3c", by.y="iso_code")
table2 <- table2[,c("name","growth.xchange.rate","growth.gov.spending")]
names(table2) <- c("Country","Growth of exchange rate CHF against LCU","Growth of government spending")

save(table2, file=paste0(data.path,"gov spending and exchange rates.Rdata"))
write.xlsx(table2, file=paste0(output.path,"Table 2 - Growth of exchange rate and government spending - Top ",range," export markets of ",countries$name[countries$un_code==country],".xlsx"), row.names = F, sheetName = "% Growth")

# TABLE 3 (AND 4)
# The third calculation relates to Swiss exports to the USA. Please calculate 
# using 2017 trade data the % and total value of Swiss exports going to the USA 
# that are in products that the Americans raised tariffs on Chinese imports during 
# 2018. I want to learn how much potential overlap there is between Swiss exports 
# and the Chinese exports hit by new American tariffs in 2018.

# The fourth calculation relates to Swiss exports to China. ??Please calculate 
# using 2017 trade data the % and total value of Swiss exports going to China 
# that are in products that the Chinese raised tariffs on US imports during 2018. 
# I want to learn how much potential overlap there is between Swiss exports and the 
# US exports hit by new Chinese tariffs in 2018.

# WHAT PRODUCTS DID USA RAISE TARIFFS AGAINST CHINA IN 2018 AND VICE VERSA

us.china <- c(840,156)
china.us <- c(156,840)

table3 <- data.frame()

# IDENTIFIED INTERVENTION IDS FOR US CHINA TRADE WAR (FROM GTA24 REPORT)
trade.war.us <- c(56890, 56823, 63051, 57917, 62073)
trade.war.chn <- c(63064, 62226, 62411)

interventions <- list(trade.war.us, trade.war.chn)

for (h in 1:2) {
  
  gta_data_slicer(gta.evaluation = c("Red","Amber"),
                  implementing.country = us.china[h],
                  keep.implementer = T,
                  affected.country = china.us[h],
                  keep.affected = T,
                  mast.chapters = "TARIFF",
                  keep.mast = T,
                  implementation.period = c("2018-01-01","2018-12-31"),
                  intervention.ids = interventions[[h]],
                  keep.interventions = T,
                  keep.implementation.na = F)
  
  products <- unique(cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=",")$affected.product)
  
  
  # WHAT SHARE / VALUE OF SWISS EXPORTS IN THESE PRODUCTS TO US OR CHINA IS AFFECTED 
  gta_trade_value_bilateral(importing.country = us.china[h],
                            keep.importer = T,
                            exporting.country = country,
                            keep.exporter = T,
                            hs.codes=gold,
                            keep.hs=F,
                            trade.data=2017)
  
  trade.base.bilateral$is.affected=trade.base.bilateral$hs6 %in% products
  
  ## that's your number
  share = sum(subset(trade.base.bilateral, is.affected)$trade.value)/sum(trade.base.bilateral$trade.value)
  
  table3 <- rbind(table3, data.frame(exporter = countries$name[countries$un_code==country],
                                     importer = countries$name[countries$un_code==us.china[h]],
                                     value = sum(subset(trade.base.bilateral, is.affected)$trade.value),
                                     type = paste0("Share affected by interventions from ",countries$name[countries$un_code==us.china[h]]),
                                     share = share)
                  )
  
}

write.xlsx(table3, file=paste0(output.path,"Table 3 - ",countries$name[countries$un_code==country]," exports to ",countries$name[countries$un_code==us.china[h]]," or ",countries$name[countries$un_code==china.us[h]]," - overlap between export hits.xlsx"),sheetName = "Coverages", row.names=F)

