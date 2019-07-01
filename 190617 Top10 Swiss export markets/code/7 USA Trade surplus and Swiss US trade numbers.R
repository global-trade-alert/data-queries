library("gtalibrary")
library("xlsx")
library("tidyverse")
library("WDI")
library("ggplot2")
library("IMFData")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")

# 1. In the meantime, for the last year that UN COMTRADE trade data is available 
# can you please identify the 10 countries with the largest trade surplus with 
# the United States? Please also report the size of the trade surplus. Please 
# treat the European Union as a single country in this calculation. If Switzerland 
# is not in the top 10 then please separately calculate the data for Switzerland and report it.

# COUNTRIES WITH LARGEST TRADE SURPLUS WITH UNITED STATES

# COUNTRIES EXPORTS TO USA:
gta_trade_value_bilateral(trade.data = 2017,
                          importing.country = 840,
                          keep.importer = T)

trade.base.bilateral$a.un[trade.base.bilateral$a.un %in% countries$un_code[countries$is.eu]] <- 999
usa.imports <- aggregate(trade.value ~ a.un, subset(trade.base.bilateral, a.un != 704), sum)
names(usa.imports) <- c("country","imports")

# ADD VIETNAM 2015 VALUES

gta_trade_value_bilateral(trade.data = 2015,
                          importing.country = 840,
                          keep.importer = T,
                          exporting.country = 704,
                          keep.exporter = T)

usa.imports <- rbind(usa.imports, data.frame(country = 704,
                                             imports = sum(trade.base.bilateral$trade.value)))

# USA EXPORTS TO WORLD

gta_trade_value_bilateral(trade.data = 2017,
                          exporting.country = 840,
                          keep.exporter = T)

trade.base.bilateral$i.un[trade.base.bilateral$i.un %in% countries$un_code[countries$is.eu]] <- 999
usa.exports <- aggregate(trade.value ~ i.un, subset(trade.base.bilateral, i.un != 704), sum)
names(usa.exports) <- c("country","exports")

# ADD VIETNAM 2015 VALUES
gta_trade_value_bilateral(trade.data = 2015,
                          importing.country = 704,
                          keep.importer = T,
                          exporting.country = 840,
                          keep.exporter = T)

usa.exports <- rbind(usa.exports, data.frame(country = 704,
                                             exports = sum(trade.base.bilateral$trade.value)))

# MERGE THE TWO AND CALCULATE DIFFERENCE
usa.surplus <- merge(usa.imports, usa.exports, by="country", all=T)
usa.surplus[is.na(usa.surplus)] <- 0
usa.surplus$surplus <- usa.surplus$imports - usa.surplus$exports

# ADD COUNTRY NAMES AND RANK
usa.surplus <- merge(usa.surplus, countries[,c("un_code","name")], by.x="country",by.y="un_code", all.x=T)
usa.surplus$name[usa.surplus$country==999] <- "EU"
usa.surplus <- usa.surplus[with(usa.surplus, order(-surplus)),]
row.names(usa.surplus) <- NULL
usa.surplus$rank <- seq(1,nrow(usa.surplus))

usa.surplus <- subset(usa.surplus, rank <= 10 | name == "Switzerland")

usa.surplus <- usa.surplus[,c("name","imports","exports","surplus","rank")]
names(usa.surplus) <- c("Country","US Imports","US exports","Trade surplus","Rank")

write.xlsx(usa.surplus, file=paste0(output.path, "USA trade surplus.xlsx"), sheetName="Surplus", row.names=F)

# 2. Please also calculate the size of the Swiss goods trade surplus with the United States 
# for each year since 2000 (again using COMTRADE data). Please also calculate the 
# share of Swiss goods exports that are sent to the United States since 2000. Please 
# also report the annual average Swiss Franc and USD exchange rate for each year 
# since 2000. (If you have access to monthly averages then for each year please 
# report the average for the year, the maximum value of the exchange rate and the 
# minimum value of the exchange rate, again each year since 2000).

load("data/support tables/Final goods support table.Rdata")
load("data/comtrade/comtrade replica 1991-2004 - all HS vintages.RData")

# SWISS TRADE SURPLUS
swiss.usa.2005 <- subset(final, Reporter.un %in% c(840, country) & Partner.un %in% c(840, country))
swiss.usa.2000 <- subset(tradedata, i.un %in% c(840, country) & a.un %in% c(840, country) & Period >= 2000)

swiss.usa.2005 <- aggregate(Value ~ Reporter.un + Partner.un + Year, swiss.usa.2005, sum)
swiss.usa.2000 <- aggregate(Trade.Value ~ i.un + a.un + Period, swiss.usa.2000, sum)

names(swiss.usa.2005) <- c("i.un","a.un","year","value")
names(swiss.usa.2000) <- c("i.un","a.un","year","value")
swiss.usa.all <- rbind(swiss.usa.2000, swiss.usa.2005)

swiss.usa.all <- spread(swiss.usa.all[,c("i.un","year","value")], i.un, value)
swiss.usa.all$surplus <- swiss.usa.all$`840`-swiss.usa.all$`756`

# SHARE OF SWISS GOODS EXPORTS TO USA
swiss.share.2005 <- subset(final, Partner.un %in% c(country))
swiss.share.2000 <- subset(tradedata, a.un %in% c(country) & Period >= 2000)

swiss.share.2005 <- swiss.share.2005[,c("Reporter.un", "Year","Value")]
swiss.share.2000 <- swiss.share.2000[,c("i.un", "Period","Trade.Value")]
names(swiss.share.2005) <- c("i.un","year","value")
names(swiss.share.2000) <- c("i.un","year","value")
swiss.share.all <- rbind(swiss.share.2000, swiss.share.2005)

swiss.share.total <- aggregate(value ~ year, swiss.share.all, sum)
swiss.share.usa <- aggregate(value ~ year, subset(swiss.share.all, i.un == 840), sum)

swiss.share.total$type <- "total"
swiss.share.usa$type <- "usa"

# SPREAD SET AND CALCULATE SHARE
swiss.share.all <- rbind(swiss.share.total, swiss.share.usa)
swiss.share.all <- spread(swiss.share.all, type, value)
swiss.share.all$share <- swiss.share.all$usa/swiss.share.all$total

swiss.share.2005.total <- aggregate(Value ~ Year, swiss.share.2005, sum) 
swiss.share.2005.usa <- aggregate(Value ~ Year, subset(swiss.share.2005, Reporter.un == 840), sum)

# CHF DOLLAR EXCHANGE RATES
xchange <- WDI(indicator="PA.NUS.FCRF", start=2000, end=2017, extra=T)
xchange=subset(xchange, country == "Switzerland")

## Add your currencies here
imf.cur=data.frame(currency=c("CHF"),
                   imf.symbol=c("CH"),
                   stringsAsFactors = F)

databaseID <- 'IFS'
startdate='2000-01-01'
enddate='2017-12-31'
checkquery = FALSE

# ANNUAL
queryfilter <- list(CL_FREQ='A',
                    CL_AREA_IFS=c("CH"),
                    CL_INDICATOR_IFS =c('ENDA_XDC_USD_RATE'))
ex.rate <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy = T)

ex.rate=ex.rate[,c(1,2)]
names(ex.rate)=c("year","annual.average")

# MONTHLY
queryfilter <- list(CL_FREQ='M',
                    CL_AREA_IFS=c("CH"),
                    CL_INDICATOR_IFS =c('ENDA_XDC_USD_RATE'))
ex.rate.month <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, checkquery, tidy = T)

ex.rate.month=ex.rate.month[,c(1,2)]
names(ex.rate.month)=c("year","monthly.average")
ex.rate.month$year <- substr(ex.rate.month$year, 0, 4)
ex.rate.month.max <- aggregate(monthly.average ~ year, ex.rate.month, max)
ex.rate.month.min <- aggregate(monthly.average ~ year, ex.rate.month, min)
names(ex.rate.month.max) <- c("year","rate.max")
names(ex.rate.month.min) <- c("year","rate.min")

ex.rates <- merge(ex.rate, merge(ex.rate.month.max, ex.rate.month.min, by="year"), by="year")

# MAKE FINAL SET
table2 <- merge(swiss.usa.all[,c("year","surplus")], swiss.share.all, by="year")
table2 <- merge(table2, ex.rates, by="year")

table2 <- table2[,c("year","total","usa","share","surplus","annual.average","rate.max","rate.min")]
names(table2) <- c("Year", "Total Swiss Trade", "Swiss exports to USA", "Share of Swiss exports to USA", "Trade surplus with USA", "Annual exchange rate", "Annual max exchange rate", "Annual min exchange rate")

write.xlsx(table2, file=paste0(output.path, "Switzerland USA export numbers.xlsx"),sheetName = "Results", row.names =F)

# 3. Finally, please identify the red+amber implemented measures in the GTA database 
# that harm only Switzerland. For each year since 2008, please calculate the share 
# of Swiss exports affected by those measures that harm only Switzerland.

gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   exporters = country,
                   keep.exporters = T,
                   nr.exporters = c(1,1),
                   incl.exporters.strictness = "ONE",
                   coverage.period = c(2008,2019))

write.xlsx(trade.coverage.estimates, file=paste0(output.path,"Swiss exports affected by targeted interventions.xlsx"),sheetName="Coverages", row.names = F)

gta_data_slicer(gta.evaluation = c("Red","Amber"),
                affected.country = country,
                keep.affected = T,
                nr.affected = c(1,1),
                incl.affected.strictness = "ONE")

interventions <- master.sliced
interventions$intervention.url <- paste0("https://www.globaltradealert.org/intervention/",interventions$intervention.id)
interventions <- unique(interventions[,c("intervention.id","title","intervention.type","date.announced","date.implemented","intervention.url","affected.product")])

write.xlsx(interventions, file=paste0(output.path, "Swiss exports - targeted interventions.xlsx"),sheetName="Interventions",row.names=F)

# CHECK IF CORRECT
# gta_data_slicer(gta.evaluation = c("Red","Amber"))
# int.one <- aggregate(intervention.id ~ a.un, master.sliced, function(x) length(unique(x)))
# int.one <- unique(master.sliced[,c("intervention.id","a.un")])
# int.one <- subset(int.one, is.na(a.un)==F)
# int.one.freq <- as.data.frame(table(int.one$intervention.id))
# int.one <- subset(int.one, intervention.id %in% int.one.freq$Var1[int.one.freq$Freq==1])
# int.one <- subset(int.one, a.un == country)
