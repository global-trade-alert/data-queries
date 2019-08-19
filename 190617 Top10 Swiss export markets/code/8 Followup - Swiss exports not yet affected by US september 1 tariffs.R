library(gtalibrary)

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")

#################
#### Request ####
#################
# SE:
# I am updating the paper I presented at the Swiss National Bank on 4 July (attached).
# 
# In June you made some calculations of the Swiss exports that might benefit from US tariffs on Chinese imports. Now that President Trump has threatened all remaining Chinese exports to the USA, can you please calculate the total value of Swiss exports that are in products that have not yet been hit by US tariffs. Please also report this value as a percentage of all Swiss exports to China. Please do these calculations with 2017 UN trade data.
# 
# Please then repeat the calculation for Swiss exports to China, again only as they relate to the products that China has not yet put tariffs on imports from the United States.
# 
# In both cases I am trying to assess the potential for Switzerland to gain should all trade between China and the United States be affected by tariffs.


# code --------------------------------------------------------------------

#personal note: It seems Patrick and Simon had a convention of not including gold which i followed 

gta_trade_value_bilateral(exporting.country='Switzerland', 
                          keep.exporter = T,
                          hs.codes = c(gold,us.affected.products),
                          keep.hs = F,
                          trade.data = '2017')

swiss.exp.unaffected.all=trade.base.bilateral

gta_trade_value_bilateral(exporting.country='Switzerland', 
                          keep.exporter = T,
                          hs.codes=gold,
                          keep.hs=F,
                          importing.country = 'China',
                          keep.importer = T,
                          trade.data = '2017')

swiss.exp.china=trade.base.bilateral

gta_trade_value_bilateral(exporting.country='Switzerland', 
                          keep.exporter = T,
                          importing.country = 'China',
                          keep.importer = T,
                          hs.codes = c(gold,us.affected.products),
                          keep.hs = F,
                          trade.data = '2017')

swiss.exp.unaffected.china=trade.base.bilateral

##find totals
swiss.exp.unaffected.all=aggregate(trade.value~a.un,swiss.exp.unaffected.all,sum)$trade.value
swiss.exp.china=aggregate(trade.value~a.un,swiss.exp.china,sum)$trade.value
swiss.exp.unaffected.china=aggregate(trade.value~a.un,swiss.exp.unaffected.china,sum)$trade.value

perc.swiss.exp.unaffected.all=paste(swiss.exp.unaffected.all/swiss.exp.china*100,'%')
perc.swiss.exp.unaffected.china=paste(swiss.exp.unaffected.china/swiss.exp.china*100,'%')


swiss.exp.unaffected.all
perc.swiss.exp.unaffected.all
perc.swiss.exp.unaffected.china

output=data.frame('Total value of Swiss exports in products not yet hit by US tariffs'=swiss.exp.unaffected.all,
                  'Percentage of total value of Swiss exports in products not yet hit by US tariffs over total swiss exports to china'=perc.swiss.exp.unaffected.all,
                  'Percentage of total value of Swiss exports to China in products not yet hit by US tariffs over total swiss exports to china'=perc.swiss.exp.unaffected.china)

writexl::write_xlsx(output,path=paste0(output.path,'Swiss trade diversion update.xlsx'))
