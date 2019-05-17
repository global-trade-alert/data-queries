library(gtalibrary)
library("xlsx")

rm(list = ls())

setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")


## JF started erroeneously: Share of own exports benefiting from own export subsidy.

### gathering intervention IDs
gta_data_slicer(affected.flows = "outward subsidy",
                gta.evaluation=c("red","amber"),
                in.force.today = T
                )

os.by.instrument=unique(master.sliced[,c("intervention.id","intervention.type", "i.un", "eligible.firms")])


## getting the HS codes
load("data/database replica/database replica - parts - base.Rdata")

products=subset(gta_affected_tariff_line, intervention_id %in% os.by.instrument$intervention.id)[,c("intervention_id","affected_products","inception_date","removal_date")]
names(products)=gsub("_","\\.", names(products))
products$inception.date=as.Date(products$inception.date, "%Y-%m-%d")
products$removal.date=as.Date(products$removal.date, "%Y-%m-%d")

products=subset(products, (removal.date>="2019-01-01"|is.na(removal.date)) &(inception.date<=Sys.Date()|is.na(inception.date)) )

## loading data
gta_trade_value_bilateral(trade.data="2017")
data.table::setnames(trade.base.bilateral, "hs6","affected.products")
total.exports=aggregate(trade.value ~ a.un, trade.base.bilateral, sum)

## abort here, what's missing is calculating the value of exports for the products in products and using gta_intervention_duration to adjust thos values to annual figures.

## the REAL query:
## Could the percentage of a nation's imports that have benefited from a foreign government's export incentive be calculated? 
## Then can a map be produced for the world using this metric?

gta_trade_coverage(gta.evaluation=c("red","amber"),
                   affected.flows = "outward subsidy",
                   mast.chapters = "L",
                   keep.mast = F,
                   coverage.period = c(2019,2019),
                    in.force.today = T,
                   trade.statistic = "value",
                   group.importers = F)

gta_trade_value_bilateral(trade.data="base")
total.imports=aggregate(trade.value ~ i.un, trade.base.bilateral,sum)
data.table::setnames(total.imports, "i.un","un_code")


benefitting.imports=trade.coverage.estimates[,c(1,4)]
names(benefitting.imports)=c("name","trade.benefitting")
benefitting.imports=merge(benefitting.imports, country.correspondence[,c("un_code", "name")], by="name", all.x=T)
benefitting.imports=merge(benefitting.imports, total.imports, by="un_code", all.x=T)
benefitting.imports$benefit.share=benefitting.imports$trade.benefitting/benefitting.imports$trade.value
