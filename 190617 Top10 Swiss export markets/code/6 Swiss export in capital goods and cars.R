library("gtalibrary")
library("xlsx")
library("tidyverse")
library("WDI")
library("ggplot2")
library("splitstackshape")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")
gta_colour_palette()


# 1. For??the last 3 years of available trade data can you please identify 
# the share of Swiss exports that are capital goods (that is, goods 
# purchased by firms as part of their investments/CAPEX).
# 
# cap.goods <- read.csv(file=paste0(data.path,"UNCTAD HS07 product groups.csv"),sep=";") # HS CODE 07, must be converted to HS12
# cap.goods <- subset(cap.goods, type == "Capital goods")$hs07
# 
# # CONVERT HS07 TO HS12
# cap.goods <- gta_hs_vintage_converter(codes = cap.goods, origin = "2007")
cap.goods=hs.codes$hs.code[hs.codes$is.capital.good]

# GET SWISS TRADE VALUES AND CALCULATE SHARES OF CAPITAL GOODS EXPORTS
trade.swiss <- data.frame()
for (i in (2017-2):2017) {
gta_trade_value_bilateral(exporting.country = country,
                          keep.exporter = T,
                          trade.data = i)
  
  trade.total <- sum(trade.base.bilateral$trade.value)
  
  trade.cap.goods <- sum(subset(trade.base.bilateral, hs6 %in% cap.goods)$trade.value)
  
  trade.swiss <- rbind(trade.swiss, data.frame(year = i,
                                               exporter = "Switzerland",
                                               total.trade = trade.total,
                                               trade.in.capital.goods = trade.cap.goods,
                                               share = trade.cap.goods/trade.total))
  
}

# SAVE FILE
write.xlsx(trade.swiss, file=paste0(output.path,"Swiss trade in capital goods.xlsx"), sheetName = "Results", row.names = F)


# 1.2) Using the existing definition of capital goods, 
# please send me the total value of Swiss exports of 
# these goods for each year since 2000.

load("data/support tables/Final goods support table.Rdata") # SUPPORT TABLE FOR GTA_TRADE_VALUE_BILATERAL DOES NOT HAVE 2005-2006 VALUES
load("data/comtrade/comtrade replica 1991-2004 - all HS vintages.RData")
ch.cap.goods.2005 <- subset(final, Partner.un %in% c(country))
ch.cap.goods.2000 <- subset(tradedata, a.un %in% c(country) & Period >= 2000)

# MAKE HS CODE LINE USABLE
ch.cap.goods.2000 <- cSplit(ch.cap.goods.2000, which(colnames(ch.cap.goods.2000)=="Commodity Code"), direction="wide", sep="-")
ch.cap.goods.2000$`Commodity Code_2` <- as.numeric(as.character(ch.cap.goods.2000$`Commodity Code_2`)) # "NA introduced by coercion" BECAUSE OF 9999AA VALUES

ch.cap.goods.2005 <- aggregate(Value ~ Year, subset(ch.cap.goods.2005, Tariff.line %in% cap.goods), sum)
# FILTER FOR CAP GOODS BEFORE 2005, NOTE TO SIMON THAT THESE MAY NOT BE 100% CORRECT
ch.cap.goods.2000 <- aggregate(Trade.Value ~ Period, subset(ch.cap.goods.2000, `Commodity Code_2` %in% cap.goods), sum)

names(ch.cap.goods.2005) <- c("Year","Value")
names(ch.cap.goods.2000) <- c("Year","Value")
ch.cap.goods.all <- rbind(ch.cap.goods.2000, ch.cap.goods.2005)

write.xlsx(ch.cap.goods.all, file=paste0(output.path,"Swiss trade in capital goods.xlsx"), sheetName = "Totals", row.names = F, append = T)


# 2. Please identify the amount of cars and car parts exports 
# that Switzerland makes to other countries around the world 
# (listing the amount per export destination.) And for each export 
# destination, please also report the share of their car and car 
# parts exports to the United States. I want to estimate the 
# exposure of Swiss car and car parts exports to new tariffs 
# by the USA on imported cars and car parts (which may happen in October.)

gta_trade_value_bilateral(exporting.country = country,
                          keep.exporter = T,
                          trade.data = 2017,
                          hs.codes = hs.car,
                          keep.hs = T)

swiss.car.trade <- aggregate(trade.value ~ i.un, trade.base.bilateral, sum)
swiss.exp.countries <- unique(swiss.car.trade$i.un)

# CAR AND CAR RELATED EXPORTS OF OTHER COUNTRIES TO USA
gta_trade_value_bilateral(exporting.country = swiss.exp.countries,
                          keep.exporter = T,
                          trade.data = 2017)

# GET TOTALS OF TRADE OF THESE COUNTRIES
total.trade.others <- aggregate(trade.value ~ a.un, trade.base.bilateral, sum)
names(total.trade.others) <- c("a.un","total.trade")

# GET THEIR SHARE OF CAR EXPORTS TO USA
trade.others.usa <- subset(trade.base.bilateral, i.un == 840 & hs6 %in% hs.car)
trade.others.usa <- aggregate(trade.value ~ a.un, trade.others.usa, sum)
names(trade.others.usa) <- c("a.un","car.trade")

# MERGE THESE TWO SETS AND CALCULATE THE SHARE OF TRADE
trade.others <- merge(total.trade.others, trade.others.usa, by="a.un", all = T)
trade.others[is.na(trade.others)] <- 0
trade.others$share.car.related.usa <- trade.others$car.trade / trade.others$total.trade

# MERGE SHARE OF OTHERS WITH SWISS CAR RELATED EXPORTS
swiss.car.trade <- merge(swiss.car.trade, trade.others[c("a.un","share.car.related.usa")], by.x="i.un", by.y="a.un", all.x=T)
swiss.car.trade <- merge(swiss.car.trade, countries[,c("name","un_code")], by.x="i.un", by.y="un_code")

# PRETTIFY TABLE
swiss.car.trade <- swiss.car.trade[,c("name","trade.value","share.car.related.usa")]
names(swiss.car.trade) <- c("Country","Swiss care related exports","Share of this country's exports car related and to USA")

write.xlsx(swiss.car.trade, file=paste0(output.path,"Swiss car related exports.xlsx"), sheetName = "Results", row.names = F)



# INVESTIGE GERMAN AND JAPANESE CAR EXPORT NUMBERS BECAUSE THEY SEEM QUITE SMALL

# German
gta_trade_value_bilateral(exporting.country = "Germany",
                          keep.exporter = T,
                          trade.data = 2017)

# Total German trade
total.german <- sum(trade.base.bilateral$trade.value)
car.german <- sum(subset(trade.base.bilateral, hs6 %in% hs.car)$trade.value)
share.overall.german <- car.german/total.german

# What countries does germany export these 19 % car related parts?
car.german.exports <- aggregate(trade.value ~ i.un, subset(trade.base.bilateral, hs6 %in% hs.car), sum)
car.german.exports$shares <- car.german.exports$trade.value/sum(car.german.exports$trade.value)
car.german.exports <- merge(car.german.exports, countries[,c("un_code","name")], by.x="i.un",by.y = "un_code")
car.german.exports <- car.german.exports[,c("name","trade.value","shares")]
car.german.exports <- car.german.exports[with(car.german.exports, order(-shares)),]
row.names(car.german.exports) <- NULL
names(car.german.exports) <- c("Importing Country","Export value in cars and car related products","Share of total car and car related exports")

german.data <- data.frame(type = c("Total Exports","Exports in cars and car related parts","Share of of exports in cars and car related parts"),
                          value = c(total.german, car.german, share.overall.german))

write.xlsx(german.data,file=paste0(output.path,"Investigate German and Japanese car related exports.xlsx"),sheetName="GER Overall",row.names=F, append =F)
write.xlsx(car.german.exports,file=paste0(output.path,"Investigate German and Japanese car related exports.xlsx"),sheetName="GER Partners",row.names=F,append=T)

# Japan
gta_trade_value_bilateral(exporting.country = "Japan",
                          keep.exporter = T,
                          trade.data = 2017)

# Total Japan trade
total.japan <- sum(trade.base.bilateral$trade.value)
car.japan <- sum(subset(trade.base.bilateral, hs6 %in% hs.car)$trade.value)
share.overall.japan <- car.japan/total.japan

# What countries does Japany export these 19 % car related parts?
car.japan.exports <- aggregate(trade.value ~ i.un, subset(trade.base.bilateral, hs6 %in% hs.car), sum)
car.japan.exports$shares <- car.japan.exports$trade.value/sum(car.japan.exports$trade.value)
car.japan.exports <- merge(car.japan.exports, countries[,c("un_code","name")], by.x="i.un",by.y = "un_code")
car.japan.exports <- car.japan.exports[,c("name","trade.value","shares")]
car.japan.exports <- car.japan.exports[with(car.japan.exports, order(-shares)),]
row.names(car.japan.exports) <- NULL
names(car.japan.exports) <- c("Importing Country","Export value in cars and car related products","Share of total car and car related exports")

japan.data <- data.frame(type = c("Total Exports","Exports in cars and car related parts","Share of of exports in cars and car related parts"),
                          value = c(total.japan, car.japan, share.overall.japan))

write.xlsx(japan.data,file=paste0(output.path,"Investigate German and Japanese car related exports.xlsx"),sheetName="JP Overall",row.names=F, append = T)
write.xlsx(car.japan.exports, file=paste0(output.path,"Investigate German and Japanese car related exports.xlsx"),sheetName="JP Partners",row.names=F, append = T)
