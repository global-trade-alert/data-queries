library(gtalibrary)

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")

#################
#### Request ####
#################
# SE:
# I am updating the paper I presented at the Swiss National Bank on 4 July (attached).
# 
# In June you made some calculations of the Swiss exports that might benefit from US tariffs on Chinese imports. 
# Now that President Trump has threatened all remaining Chinese exports to the USA, can you please calculate the total value of Swiss exports that are in products that have not yet been hit by US tariffs. Please also report this value as a percentage of all Swiss exports to China. Please do these calculations with 2017 UN trade data.
# 
# Please then repeat the calculation for Swiss exports to China, again only as they relate to the products that China has not yet put tariffs on imports from the United States.
# 
# In both cases I am trying to assess the potential for Switzerland to gain should all trade between China and the United States be affected by tariffs.


# code --------------------------------------------------------------------

gta_trade_value_bilateral(exporting.country='Switzerland', 
                                        keep.exporter = T,
                                        trade.data = '2017')
swiss.exports=trade.base.bilateral

# value of non-affected trade
value.ch.exports=sum(subset(swiss.exports, ! hs6 %in% c(gold))$trade.value)
value.ch.exports.not.hit=sum(subset(swiss.exports, ! hs6 %in% c(gold,us.affected.products))$trade.value)
share.ch.exports.not.hit=value.ch.exports.not.hit/value.ch.exports
scales::percent(share.ch.exports.not.hit)

value.ch.exports.chn=sum(subset(swiss.exports, ! hs6 %in% c(gold) & i.un==156)$trade.value)
value.ch.exports.chn.not.hit=sum(subset(swiss.exports, ! hs6 %in% c(gold,us.affected.products) & i.un==156)$trade.value)
share.ch.exports.chn.not.hit=value.ch.exports.chn.not.hit/value.ch.exports.chn
scales::percent(share.ch.exports.chn.not.hit)

value.ch.exports.us=sum(subset(swiss.exports, ! hs6 %in% c(gold) & i.un==840)$trade.value)
value.ch.exports.us.not.hit=sum(subset(swiss.exports, ! hs6 %in% c(gold,us.affected.products) & i.un==840)$trade.value)
share.ch.exports.us.not.hit=value.ch.exports.us.not.hit/value.ch.exports.us
scales::percent(share.ch.exports.us.not.hit)

output=data.frame(exporter='Switzerland',
                  importer=c('World','China','USA'),
                  'Absolute value of unaffected product exports'= c(value.ch.exports.not.hit,value.ch.exports.chn.not.hit,value.ch.exports.us.not.hit),
                  'Share of unaffected products in total exports'=c(share.ch.exports.not.hit,share.ch.exports.chn.not.hit,share.ch.exports.us.not.hit))

writexl::write_xlsx(output,path=paste0(output.path,'Swiss trade diversion update.xlsx'))
