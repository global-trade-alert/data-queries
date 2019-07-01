library("gtalibrary")
library("xlsx")
library("tidyverse")
library("WDI")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")


# GET TOP MARKETS AND SECTORS
load(paste0(data.path,"Top CPC and markets.Rdata"))
top.cpc <- top.cpc$cpc
top.markets <- top.markets$un_code

# PREPARE DATASET
nr.of.tables = 5
counter = 1

results <- data.frame(cpc = rep(top.cpc, times=range),
                      country = rep(top.markets, each=range))
results <- merge(results, countries[,c("un_code","name")], by.x="country", by.y="un_code")
results$country <- NULL


# QUERY

# The first calculations relate to the top 10 foreign markets for Swiss products and 
# the top 10 sectors (excluding gold from identification of both.) You have already 
# identified the top 10 foreign markets and top 10 sectors (excluding gold). Think 
# in terms of a matrix, with the foreign markets along one dimension and the sectors 
# along another dimension. The matrix will have 100 cells. Please calculate for each cell:
#   

# 1. The % of Swiss exports that face a trade distortion (red/amber implemented) 
# in force in 2010 implemented by the importing country.
# 3. The % of Swiss exports that face a trade distortion in force in 2017 implemented 
# by the importing country.

results.temp <- data.frame()
  
for (i in top.cpc) {
  print(paste0("--- ",counter, " / ", range*nr.of.tables," ---"))
  counter = counter + 1
  
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                    coverage.period = c(2010,2017),
                    exporters = country,
                    keep.exporters = T,
                    importers = top.markets,
                    keep.importers = T,
                    group.importers = F,
                    cpc.sectors = i,
                    keep.cpc = T,
                    implementer.role = "Importer")
  
  if (exists("trade.coverage.estimates")) {
    results.temp <- rbind(results.temp, data.frame(cpc = i,
                                         name = trade.coverage.estimates[,c("Importing country")],
                                         distortions.2010 = trade.coverage.estimates[,which(grepl("2010", names(trade.coverage.estimates)))],
                                         distortions.2017 = trade.coverage.estimates[,which(grepl("2017", names(trade.coverage.estimates)))],
                                         stringsAsFactors = F))
    rm(trade.coverage.estimates)
  }
}

results <- merge(results, results.temp, by=c("cpc","name"), all.x = T)

# 2. The % of??Swiss exports that compete against an export incentive (P7) in 2010 
# implemented by a third country that affects the importing country in question.
# 4. The % of??Swiss exports that compete against an export incentive (P7) in 2017 
# implemented by a third country that affects the importing country in question.

results.temp <- data.frame()
export.subsidies=int.mast.types$intervention.type[int.mast.types$mast.subchapter.id=="P7"]

for (i in top.cpc) {
  print(paste0("--- ",counter, " / ", range*nr.of.tables," ---"))
  counter = counter + 1
  
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     affected.flows = "outward subsidy",
                     coverage.period = c(2010,2017),
                     exporters = country,
                     keep.exporters = T,
                     importers = top.markets,
                     keep.importers = T,
                     group.importers = F,
                     cpc.sectors = i,
                     keep.cpc = T,
                     intervention.types = export.subsidies,
                     keep.type = T,
                     implementer.role = "3rd country")
  
  if (exists("trade.coverage.estimates")) {
    results.temp <- rbind(results.temp, data.frame(cpc = i,
                                                   name = trade.coverage.estimates[,c("Importing country")],
                                                   export.incentives.2010 = trade.coverage.estimates[,which(grepl("2010", names(trade.coverage.estimates)))],
                                                   export.incentives.2017 = trade.coverage.estimates[,which(grepl("2017", names(trade.coverage.estimates)))],
                                                   stringsAsFactors = F))    
    rm(trade.coverage.estimates)
  }
}

results <- merge(results, results.temp, by=c("cpc","name"), all.x = T)


# 5. The ratio of Swiss exports in 2017 to 2010.
results.temp <- data.frame()

for (i in top.cpc) {
  print(paste0("--- ",counter, " / ", range*nr.of.tables," ---"))
  counter = counter + 1
  
  #2010
  gta_trade_value_bilateral(exporting.country = country,
                            importing.country = top.markets,
                            keep.exporter = T,
                            keep.importer = T,
                            cpc.sectors = i,
                            keep.cpc = T,
                            trade.data = 2010)
  
  trade.data.2010 = aggregate(trade.value ~ i.un, trade.base.bilateral, sum)
  
  #2017
  gta_trade_value_bilateral(exporting.country = country,
                            importing.country = top.markets,
                            keep.exporter = T,
                            keep.importer = T,
                            cpc.sectors = i,
                            keep.cpc = T,
                            trade.data = 2017)
  
  trade.data <- merge(trade.data.2010, aggregate(trade.value ~ i.un, trade.base.bilateral, sum), by="i.un")
  trade.data <- merge(trade.data, countries[,c("name","un_code")], by.x="i.un", by.y="un_code")
  
  results.temp <- rbind(results.temp, data.frame(cpc = i,
                                                 name = trade.data$name,
                                                 trade.ratio.2017.2010 = trade.data$trade.value.y/trade.data$trade.value.x))
  
}

results <- merge(results, results.temp, by=c("cpc","name"), all.x = T)
results$trade.ratio.2017.2010[is.na(results$trade.ratio.2017.2010)]="not exported"


####################
# CALCULATE 2017 VALUES BUT WITH RESTRICTION THAT EXPORT 
# SHARES MUST BE AFFECTED BY 3 OR MORE INTERVENTIONS
####################

# All distortions
results.temp <- data.frame()

for (i in top.cpc) {
  print(paste0("--- ",counter, " / ", range*nr.of.tables," ---"))
  counter = counter + 1
  
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     coverage.period = c(2017,2017),
                     exporters = country,
                     keep.exporters = T,
                     importers = top.markets,
                     keep.importers = T,
                     group.importers = F,
                     cpc.sectors = i,
                     keep.cpc = T,
                     implementer.role = "Importer",
                     hit.brackets = c(3,999999))
  
  if (exists("trade.coverage.estimates")) {
    results.temp <- rbind(results.temp, data.frame(cpc = i,
                                                   name = trade.coverage.estimates[,c("Importing country")],
                                                   distortions.2017.hit3orMore = trade.coverage.estimates[,which(grepl("2017", names(trade.coverage.estimates)))],
                                                   stringsAsFactors = F))
    rm(trade.coverage.estimates)
  }
}

results <- merge(results, results.temp, by=c("cpc","name"), all.x = T)

# Export incentives
results.temp <- data.frame()
export.subsidies=int.mast.types$intervention.type[int.mast.types$mast.subchapter.id=="P7"]

for (i in top.cpc) {
  print(paste0("--- ",counter, " / ", range*nr.of.tables," ---"))
  counter = counter + 1
  
  gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                     affected.flows = "outward subsidy",
                     coverage.period = c(2017,2017),
                     exporters = country,
                     keep.exporters = T,
                     importers = top.markets,
                     keep.importers = T,
                     group.importers = F,
                     cpc.sectors = i,
                     keep.cpc = T,
                     intervention.types = export.subsidies,
                     keep.type = T,
                     implementer.role = "3rd country",
                     hit.brackets = c(3,999999))
  
  if (exists("trade.coverage.estimates")) {
    results.temp <- rbind(results.temp, data.frame(cpc = i,
                                                   name = trade.coverage.estimates[,c("Importing country")],
                                                   export.incentives.2017.hit3orMore = trade.coverage.estimates[,which(grepl("2017", names(trade.coverage.estimates)))],
                                                   stringsAsFactors = F))    
    rm(trade.coverage.estimates)
  }
}

results <- merge(results, results.temp, by=c("cpc","name"), all.x = T)
results[is.na(results)]=0

# SAVE RESULTS
save(results, file=paste0(data.path, "results matrix.Rdata"))
load(paste0(data.path,"results matrix.Rdata"))


# The idea here is as follows: I want to relate the change/growth in exports to the 
# changes in the exposure to import distortions and to third party export incentives).

# CREATE MATRIX TABLES

colnms <- names(results)
colnms <- colnms[! colnms %in% c("cpc","name")]

results = merge(results, cpc.names, by="cpc")

results <- results[,c("name","cpc.name",colnms)]

for (t in 3:(length(colnms)+2)){
  results.temp = results[,c(1,2,t)]
  results.temp = spread(results.temp, name, 3)
  # result.xlsx <- results.temp[,-1]
  # rownames(result.xlsx) <- results.temp[,1]
  xlsx::write.xlsx(results.temp, file=paste0(output.path,"Matrix CPC and export markets of ", countries$name[countries$un_code==country],".xlsx"), append = T, sheetName = gsub("\\.", " ",colnms[t-2]), row.names=F)
}


# MAKE CHANGES OF MATRIX 1-4 IN LONG FORMAT
results.long <- results
results.long$difference.distortions <- results.long$distortions.2017-results.long$distortions.2010
results.long$difference.export.incentives <- results.long$export.incentives.2017-results.long$export.incentives.2010
results.long$log.export.ratio <- log(as.numeric(results.long$trade.ratio.2017.2010))
results.long <- subset(results.long, trade.ratio.2017.2010 != "not exported")

results.long <- results.long[,c("name","cpc.name","difference.distortions","difference.export.incentives","log.export.ratio")]

write.xlsx(results.long, file=paste0(output.path,"Differences 2017-2010.xlsx"), row.names=F, sheetName="Differences")

# ADD GOV SPENDING AND EXCHANGE RATE
load(paste0(data.path,"gov spending and exchange rates.Rdata"))

results.long <- merge(results.long, table2, by.x = "name", by.y = "Country")

save(results.long, file=paste0(data.path,"results long.Rdata"))
write.xlsx(results.long, file=paste0(output.path,"Differences 2017-2010.xlsx"), row.names=F, sheetName="Differences", append = T)


