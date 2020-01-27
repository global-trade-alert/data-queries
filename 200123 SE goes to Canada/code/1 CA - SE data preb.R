rm(list=ls())
library(gtalibrary)
library(gtasql)
library(tidyverse)
library(splitstackshape)
library(lubridate)
library(PRMTools)

gta_setwd()

wd.path = "4 data queries/200123 SE goes to Canada/"
output.path = paste0(wd.path, "results/")
data.path = paste0(wd.path,"data/")

# DEFINITIONS
spez.country <- list("Canada", 124) # CANADA
world <- list("World", gtalibrary::country.names$un_code)
instruments <- gtalibrary::int.mast.types
instr <- list(list("Any", instruments$intervention.type),
              list("Import tariff", subset(instruments, mast.chapter.id == "TARIFF")$intervention.type),
              list("Subsidies", subset(instruments, mast.chapter.id == "L")$intervention.type),
              list("Export subsidies", subset(instruments, intervention.type %in% instruments$intervention.type[instruments$is.export.promotion])$intervention.type))

   
# Global exports affected, stacked up by nr of hits. For World and Canada
exports.hits.results <- data.frame()

for(c in list(spez.country, world)){
  
  gta_trade_coverage(
    gta.evaluation = c("Red","Amber"),
    exporters = c[[2]],
    keep.exporters = T,
    group.exporters = T,
    hit.brackets = c(1,1,2,2,3,5,6,10,11,999999),
    coverage.period = c(2009,2020)
  )
  exports.hits.results <- rbind(exports.hits.results, data.frame(exporter=c[[1]],
                                                                 hits=trade.coverage.estimates[,ncol(trade.coverage.estimates)-1],
                                                                 share=trade.coverage.estimates[,ncol(trade.coverage.estimates)]))  
}


# line chart for share of global exports affected, 
# one line per following instrument: any, import tariff, subsidies (MAST L), export subsidies.

int.types.results <- data.frame()

for(c in list(spez.country, world)){
  
  for (int in instr) {
    
    gta_trade_coverage(
      gta.evaluation = c("Red","Amber"),
      exporters = c[[2]],
      keep.exporters = T,
      group.exporters = T,
      intervention.types = int[[2]],
      keep.type = T,
      coverage.period = c(2009,2020)
    )
    int.types.results <- rbind(int.types.results, data.frame(exporter=c[[1]],
                                                             type=int[[1]],
                                                             share=trade.coverage.estimates[,ncol(trade.coverage.estimates)]))  
  }
}

# If possible: Update GTA 25's Chapter 3 Figure 3 (attached) to 
# December  for each period (ie. 36 monthly estimates instead of 35)

############### COPIED CODE FROM GTA25/single and multiple nations affected
trade.data.year = "base"
cutoff <- c("2019-12-31")

# LIST OF PERIODS TO BE ITERATED
periods <- list(c("2017-01-01",cutoff),c("2014-01-01","2016-12-31"),c("2009-01-01","2011-12-31"))

# LIST OF MAST CHAPTERS TO BE ITERATED AND IF THEY SHOULD BE KEPT OR NOT
mast.chapters <- list(c("TARIFF",T), c("TARIFF",F))
export.subsidies=int.mast.types$intervention.type[int.mast.types$is.export.promotion==1]

single.multi.data=data.frame()

for(period in 1:length(periods)){
  period.start=as.Date(periods[[period]][[1]])
  period.end=as.Date(periods[[period]][[2]])
  period.months=seq.Date(period.start, period.end, by = "month")
  
  
  for(monat in period.months){
    monat=as.Date(monat, origin="1970-01-01")
    
    print(paste("starting",format(monat, "%y-%m")))
          
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         affected.flows = c("outward subsidy", "inward"),
                         coverage.period = c(year(monat),year(monat)),
                         implementation.period = c(period.start,(monat+months(1)-1)),
                         trade.data = trade.data.year)
      
      
      if(exists("trade.coverage.estimates")){
        
        trade.value=as.numeric(trade.coverage.estimates[,ncol(trade.coverage.estimates)])
        rm(trade.coverage.estimates)
      } else {
        
        trade.value=0  
      }
      
      single.multi.data=rbind(single.multi.data,
                              data.frame(period.id=period,
                                         month=format(monat, "%y-%m"),
                                         month.count=mondf(period.start, monat),
                                         instrument="all",
                                         target="all",
                                         trade.share=trade.value,
                                         stringsAsFactors = F))
      
  }
}


# Export opportunities (last slide in UK ppt): Idea is to identify secotrs/destinations with largest 
# liberalisations in recent past. See final section of code "UK - SE talking points - data prep.R" 
# in code folder for how I did this.

## trade data
gta_trade_value_bilateral(exporting.country = spez.country[[2]], keep.exporter = T, trade.data="2018")
country.trade=merge(trade.base.bilateral, cpc.to.hs, by.x="hs6", by.y="hs")
country.trade.agg=aggregate(trade.value ~ cpc, country.trade, sum)
country.trade.agg=country.trade.agg[order(-country.trade.agg$trade.value),]
rm(trade.base.bilateral)

nr.sectors=20
new.opportunities=data.frame()

for(i in 1:20){
  this.cpc=country.trade.agg$cpc[i]
  gta_trade_coverage(exporters = spez.country[[2]], 
                     group.importers = F,
                     coverage.period = c(2018,2019),
                     keep.exporters = T, 
                     implementation.period = c("2018-01-01","2019-12-31"),
                     gta.evaluation = c("Green"),
                     implementer.role = "importer",
                     trade.statistic = "value",
                     trade.data = "2017",
                     intra.year.duration = T,
                     group.mast = T,
                     cpc.sectors = this.cpc,
                     keep.cpc = T ,
                     in.force.today="yes")
  
  new.opportunities=rbind(new.opportunities, 
                          data.frame(cpc=this.cpc,
                                     importer=trade.coverage.estimates$`Importing country`,
                                     year.2018=trade.coverage.estimates$`Trade coverage estimate for 2018`,
                                     year.2019 = trade.coverage.estimates$`Trade coverage estimate for 2019`,
                                     stringsAsFactors = F))
  
}

save(new.opportunities, single.multi.data, int.types.results, exports.hits.results)
