library(gtalibrary)
library("xlsx")

rm(list = ls())

setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")

ijs=c("Brazil","China","India","Russia","EU-28","G20", "United States of America")

export.exposure=data.frame()
for(ij in ijs){
  
  gta_trade_coverage(gta.evaluation=c("Red","Amber"),
                     implementers=ij,
                     affected.flow="outward subsidy",
                     group.exporters = F)
  
  trade.coverage.estimates$`Importing country`="World"
  trade.coverage.estimates$implementer=ij
  export.exposure=rbind(export.exposure,trade.coverage.estimates)
  
  print(paste("DONE WITH WORLD FOR", ij))
  
  gta_trade_coverage(gta.evaluation=c("Red","Amber"),
                     implementers=ij,
                     affected.flow="outward subsidy",
                     group.exporters = F,
                     group.importers=F)
  
  trade.coverage.estimates$implementer=ij
  export.exposure=rbind(export.exposure,trade.coverage.estimates)
  
  print(paste("DONE WITH", ij))
  
}

names(export.exposure)=c("importer","exporter","nr_interventions", paste("year_",2009:2019,sep=""), "implementer")
export.exposure=export.exposure[,c( "implementer","exporter","importer", paste("year_",2009:2019,sep=""))]

write.csv(export.exposure, file="4 data queries/190501 Matteo Fiorini/GTA ES data.csv", row.names = F)
save(export.exposure, file="4 data queries/190501 Matteo Fiorini/GTA ES data.Rdata")
