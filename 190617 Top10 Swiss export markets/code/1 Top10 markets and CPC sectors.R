library("gtalibrary")
library("xlsx")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")

# QUERY

# Using the latest year of international trade data available, 
# please send me an excel sheet that lists the top 10 largest foreign 
# markets for Swiss exports (and the share of overall Swiss exports). 
# Please also send me a sheet with the top 10 sectors of Swiss exports 
# (and??the share of overall Swiss exports).


# LOAD COUNTRY NAMES
countries <- gtalibrary::country.names

# GET SWISS EXPORT VALUES
gta_trade_value_bilateral(exporting.country = country,
                          keep.exporter = T,
                          hs.codes = gold,
                          keep.hs = F,
                          trade.data = 2017)

total.export <- sum(trade.base.bilateral$trade.value)

#------------------------

# TOP 10 EXPORT MARKETS

#------------------------
top.markets <- aggregate(trade.value ~ i.un, trade.base.bilateral, sum)
total1 <- sum(top.markets$trade.value)

# CUT TO TOP RANGE AND MERGE COUNTRY NAMES
names(top.markets) <- c("un_code","value")
top.markets <- merge(top.markets, countries[,c("name","un_code")], by="un_code")
top.markets <- top.markets[with(top.markets, order(-value)),]
row.names(top.markets) <- NULL
top.markets <- top.markets[1:range,]

#ADD SHARE
top.markets$share <- top.markets$value/total.export

# SAVE XLSX
top.markets.xlsx <- top.markets[,c("name","value","share")]
names(top.markets.xlsx) <- c("Importing Country","Trade Value","Share of total exports")
write.xlsx(top.markets.xlsx, file=paste0(output.path,"Top ",range," export markets of ",countries$name[countries$un_code==country],".xlsx"), sheetName=paste0("Top",range), row.names=F)


#------------------------

# TOP 10 CPC SECTORS

#------------------------
top.cpc <- aggregate(trade.value ~ hs6, trade.base.bilateral, sum)
sum(top.cpc$trade.value)

# MERGE CPC SECTOR CODES
conversion <- gtalibrary::cpc.to.hs
names(top.cpc) <- c("hs","value")
top.cpc <- merge(top.cpc, conversion, by="hs", keep.all=T)
top.cpc <- aggregate(value ~ cpc, top.cpc, sum)
total2 <- sum(top.cpc$value)
total1-total2 # NOT THE SAME BECAUSE OF HS CODE 999999

# SORT VALUES AND ADD CPC NAMES
cpc <- subset(gtalibrary::cpc.names, cpc.digit.level ==3)
top.cpc <- merge(top.cpc, cpc[,c("cpc","cpc.name")], by="cpc")
top.cpc <- top.cpc[with(top.cpc, order(-value)),]
row.names(top.cpc) <- NULL
top.cpc <- top.cpc[1:range,]

# ADD SHARE
top.cpc$share <- top.cpc$value/total.export

# SAVE XLSX
top.cpc.xlsx <- top.cpc[,c("cpc","cpc.name","value","share")]
names(top.cpc.xlsx) <- c("CPC Code","CPC Name","Trade Value","Share of total exports")
write.xlsx(top.cpc.xlsx, file=paste0(output.path,"Top ",range," CPC sectors of ",countries$name[countries$un_code==country],".xlsx"), sheetName=paste0("Top",range), row.names=F)

save(top.cpc, top.markets, file=paste0(data.path,"Top CPC and markets.Rdata"))
