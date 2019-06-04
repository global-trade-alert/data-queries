library("gtalibrary")
library("xlsx")
library("splitstackshape")

rm(list=ls())

gta_setwd()

## IDs of untargeted interventions by CHN and US in MAST P
gta_data_slicer(gta.evaluation = c("red","amber"),
                implementing.country = c("China", "United States of America"),
                keep.implementer = T,
                keep.implementation.na = F,
                mast.chapters = "P",
                keep.mast = T,
                affected.flows ="outward subsidy")
unique(master.sliced$intervention.type)
unique(master.sliced$implementing.jurisdiction)
p.interventions=unique(master.sliced$intervention.id)

# Identifying cases where the US or China is the distorted market
load("data/database replica/database replica - parts - base.Rdata")

p.cases=subset(gta_tuple, intervention_id %in% p.interventions & un_code_distorted %in% c(156,840))

unique(p.cases$un_code_implementer)
p.cases=unique(p.cases[,c("intervention_id","un_code_distorted","affected_products")])
p.cases=subset(p.cases, is.na(affected_products)==F)
names(p.cases)=c("intervention.id","i.un","hs6")

## adding base year trade values
gta_trade_value_bilateral(importing.country = c(840,156),
                          keep.importer = T,
                          exporting.country = c(840, 156),
                          keep.exporter = T)
p.cases=merge(p.cases, trade.base.bilateral[,c("i.un","hs6","trade.value")], by=c("i.un","hs6"))

## getting the intervention duration for each year
gta_intervention_duration(data.path='master.sliced[,c("intervention.id", "date.implemented", "date.removed")]', 
                          is.data.frame = T,
                          years=c(2009,2019),
                          df.name="p.durations")
p.cases=merge(p.cases, p.durations, by="intervention.id")

p.cases$trade.affected=p.cases$trade.value*p.cases$share

# summing across inteventions (to avoid double counting affected tariff lines) 
# [I know this is sloppy since I assume overlapping implementation periods; but that biases the numbers downward, which I am fine with.]
trade.coverage=aggregate(trade.affected ~ i.un + year + hs6, p.cases, max)

trade.coverage.estimates=merge(aggregate(trade.affected ~ i.un + year, trade.coverage, sum), 
                               aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un", all.x=T)


trade.coverage.estimates$trade.share=trade.coverage.estimates$trade.affected/trade.coverage.estimates$trade.value

trade.coverage.estimates=trade.coverage.estimates[c("i.un","year","trade.share")]
names(trade.coverage.estimates)=c("importer","year","value")
trade.coverage.estimates$exporter="China"
trade.coverage.estimates$exporter[trade.coverage.estimates$importer==156]="United States of America"

trade.coverage.estimates$importer="China"
trade.coverage.estimates$importer[trade.coverage.estimates$exporter=="China"]="United States of America"
trade.coverage.estimates$mast="P"
trade.coverage.estimates$target.status="Any"


save(trade.coverage.estimates, file="4 data queries/190604 Simon Panel US-Sino tariff war/data/P estimates.Rdata")
