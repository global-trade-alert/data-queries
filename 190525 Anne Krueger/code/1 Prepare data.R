library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")

rm(list=ls())

setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")

output.path <- "4 data queries/190525 Anne Krueger/output/"



# 1. Is there a list of the trade remedy actions in effect by the 
# US against China for a recent year? Is there an estimate of the 
# average height of the dumping margins or any other useful indicator 
# of how high they were/are?

# GET LIST OF INTERVENTIONS
interventions <- read.csv("data/database replica/gta_intervention.csv",sep=",")

# GET INTERVENTION IDS FOR RECENT YEAR US AGAINST CHINA MAST CHAPTER D
gta_data_slicer(gta.evaluation = c("Red","Amber"),
                mast.chapters = "D",
                keep.mast = T,
                implementing.country = 840,
                keep.implementer = T,
                affected.country = 156,
                keep.implementation.na = F,
                keep.affected = T,
                keep.others = F,
                in.force.today = T)

master <- unique(master.sliced[,c("intervention.type", "intervention.id", "state.act.id", "title", "date.announced", "date.implemented", "date.removed", "gta.evaluation", "implementing.jurisdiction", "affected.sector", "affected.product", "affected.jurisdiction", "mast.id", "mast.chapter")])

# ADD URL
master$gta.url <- paste0("https://www.globaltradealert.org/state-act/", master$state.act.id)

# REMOVE UNNECESSARY COLUMNS
master.xlsx <- master
master.xlsx[,c("intervention.id", "state.act.id")] <- NULL

# SAVE EXCEL OF INTERVENTIONS
write.xlsx(master.xlsx, file=paste0(output.path,"Trade remedy actions by USA against China, in force today.xlsx"), sheetName = "Interventions", row.names=F)

# GET INTERVENTION DESCRIPTIONS
intervention.ids <- unique(master$intervention.id)
interventions <- subset(interventions, id %in% intervention.ids)

# SEARCH REGULAR EXPRESSION FOR PERCENTAGES
interventions$description <- as.character(interventions$description)
for(i in 1:nrow(interventions)) {
  interventions$str.extract[i] <- ifelse(length(unlist(str_extract_all(interventions$description[i], "(?<![A-Za-z0-9.\\S])[0-9.\\-]+(\\%)")))>0, paste(unlist(str_extract_all(interventions$description[i], "(?<![A-Za-z0-9.\\S])[0-9.\\-]+(\\%)")), collapse=", "),NA)
}
# NOT MUCH DATA ON DUMPING MARGINS AVAILABLE...



# 2. Is there a calculation of how much of Chinese steel was 
# already subject AD and CVD tariffs before the steel tariffs 
# went generally into effect? Is there an estimate of the average 
# dumping margin that was in effect for steel?

# TRUMP'S STEEL TARIFFS TAKEN FROM GTA23 HELP FILE
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")
tw.sa.us=c(27158, 27214, 28096, 27215, 27906)

# GET LIST OF INTERVENTIONS
gta_data_slicer(gta.evaluation = c("Red","Amber"),
                affected.country = 156,
                implementing.country = 840,
                keep.affected = T)

# GET INTERVENTIONS WHICH BELONG TO TRADE WAR IMPLEMENTED BY US
interventions <- subset(master.sliced, state.act.id %in% tw.sa.us)

# WHEN WAS THE FIRST OF THESE INTERVENTIONS IMPLEMENTED?
start.t.tariffs <- min(as.Date(interventions$date.implemented))

# STEEL HS CODES, TAKEN FROM "GTA22/Annex/2 Steel trade exposure_new.R"
steel.hs4 <- c(7206:7215, 7217:7229, 7304, 7306)
steel.hs6 <- c(721610, 721621, 721622, 721631:721633, 721640, 721650, 730110, 730210, 730240, 730721:730723, 730729, 730791:730793, 730799)

# CALCULATE COVERAGES FOR INTERVENTIONS BEFORE THE FIRST TRUMP TARIFF
gta_trade_coverage(gta.evaluation=c("Red","Amber"),
                   mast.chapters = c("D"),
                   group.mast = F,
                   keep.mast = T,
                   hs.codes = steel.hs6,
                   keep.hs = T,
                   importers = 840,
                   keep.importers = T,
                   exporters = 156,
                   keep.exporters = T,
                   implementer.role = "importer",
                   implementation.period = c("2008-01-01", paste(start.t.tariffs-1)),
                   coverage.period = c(2009, 2019)
)

trade.coverage.estimates$`Number of interventions affecting exported product` <- NULL
trade.coverage.estimates <- trade.coverage.estimates[2,]

# PRETTIFY XLSX
trade.coverage.estimates$`Importing country` <- "USA"
trade.coverage.estimates$`Exporting country` <- "China"

# WRITE XLSX
write.xlsx(trade.coverage.estimates, file=paste0(output.path,"Trade coverages for Chinese steel exports to USA.xlsx"), sheetName = "Coverages", row.names=F)



# 3. Do you have a good recent estimate of other percentage of 
# US imports subject to trade remedy tariff margins? It could be 
# any measure: how much of earlier imports were covered or current 
# coverage; and I can live without either, but if it's easy to 
# get it would be good.

gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   importers = 840,
                   group.importers = F,
                   mast.chapters = c("D"),
                   keep.mast = T,
                   group.mast = F,
                   keep.importers = T,
                   implementer.role = "importer")


trade.coverage.estimates$`Number of interventions affecting exported product` <- NULL
trade.coverage.estimates <- trade.coverage.estimates[2,]

# PRETTIFY XLSX
trade.coverage.estimates$`Importing country` <- "USA"
trade.coverage.estimates$`Exporting country` <- "World"

# WRITE XLSX
write.xlsx(trade.coverage.estimates, file=paste0(output.path,"Trade coverages for US imports.xlsx"), sheetName = "Coverages", row.names=F)

