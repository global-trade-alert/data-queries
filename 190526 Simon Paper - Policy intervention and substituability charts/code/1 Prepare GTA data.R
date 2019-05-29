library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")

rm(list=ls())

gta_setwd()

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")

#---------------------------------------------#
#                                             #
#   PERCENTAGE DATASETS                       #
#                                             #
#---------------------------------------------#

# FOR EACH PERIOD

# PREPARE SETS
harmful.set <- data.frame()

traditional.set <- data.frame()

subsidy.set <- data.frame()

for (p in 1:length(periods)) {
  
  gta_data_slicer(implementation.period = c(periods[[p]][1], periods[[p]][2]),
                  keep.implementation.na = F)
  
  for(g in 1:length(groups)) {
    
    print(paste0("Period: ",p))
    
    # HARMFUL 
    # Let "% harmful" refer to the total number of red or amber implemented measures 
    # as a % of the total number of implemented measures by a jurisdiction.
    
    harmful <- subset(master.sliced, i.un %in% groups[[g]])
    
    # SET HARMFUL AND LIBERALISING
    harmful$gta.evaluation[harmful$gta.evaluation != "Green"] <- "Harmful"
    harmful$gta.evaluation[harmful$gta.evaluation == "Green"] <- "Liberalising"
    harmful$year <- year(harmful$date.implemented)
    
    # AGGREGATE FOR EVALUATION AND YEAR
    harmful <- aggregate(intervention.id ~ gta.evaluation + i.un, harmful, function(x) length(unique(x)))
    harmful <- spread(harmful, gta.evaluation, intervention.id)
    
    harmful[is.na(harmful)] <- 0
    
    # CALCULATE PERCENTAGES
    harmful$total <- harmful$Harmful + harmful$Liberalising
    harmful$harmful.percentage <- harmful$Harmful/harmful$total
    
    # RBIND SET
    harmful.set <- rbind(harmful.set, data.frame(name = groups.name[g],
                                                 i.un=harmful$i.un,
                                                 harmful.percentage = harmful$harmful.percentage,
                                                 period = p,
                                                 stringsAsFactors = F)
    )
    
    
    # TRADITIONAL
    # Let "% traditional" refer to the total number of red and amber implemented 
    # tariff increases, MAST chapter D, and MAST subchapters E1, E2, E5, E6, and E9 
    # as a % of all red and amber implemented measures by a jurisdiction.
    
    traditional <- subset(master.sliced, i.un %in% groups[[g]])
    
    # SET MAST CHAPTER TO MAST AND OTHER
    traditional$mast.chapter <- as.character(traditional$mast.chapter)
    traditional$mast <- "Other"
    traditional$mast[traditional$mast.id %in% traditional.types] <- "MAST"
    
    
    # AGGREGATE ON BASIS OF MAST CHAPTER AND YEAR
    traditional <- aggregate(intervention.id ~ mast + i.un, traditional, function(x) length(unique(x)))
    traditional <- spread(traditional, mast, intervention.id)
    
    traditional[is.na(traditional)] <- 0
    
    # CALCULATE PERCENTAGES
    traditional$total <- traditional$Other + traditional$MAST
    traditional$traditional.percentage <- traditional$MAST/traditional$total
    
    # RBIND SET
    traditional.set <- rbind(traditional.set, data.frame(name = groups.name[g],
                                                         i.un=traditional$i.un,
                                                         traditional.percentage = traditional$traditional.percentage,
                                                         period = p,
                                                         stringsAsFactors = F)
    )
    
    # SUBSIDY
    # Let "% subsidy" refer to the total number of red and amber implemented MAST 
    # chapter L, P7 and P8 measures as a % of all red and amber implemented 
    # measures by a jurisdiction.
    subsidy <-  subset(master.sliced, i.un %in% groups[[g]])
    
    # SET MAST CHAPTER TO MAST AND OTHER
    subsidy$mast.chapter <- as.character(subsidy$mast.chapter)
    subsidy$mast <- "Other"
    subsidy$mast[subsidy$mast.id %in% subsidy.types] <- "MAST"
    
    
    # AGGREGATE ON BASIS OF MAST CHAPTER AND YEAR
    subsidy <- aggregate(intervention.id ~ mast + i.un, subsidy, function(x) length(unique(x)))
    subsidy <- spread(subsidy, mast, intervention.id)
    
    subsidy[is.na(subsidy)] <- 0
    
    # CALCULATE PERCENTAGES
    subsidy$total <- subsidy$Other + subsidy$MAST
    subsidy$subsidy.percentage <- subsidy$MAST/subsidy$total
    
    # RBIND SET
    subsidy.set <- rbind(subsidy.set, data.frame(name = groups.name[g],
                                                 i.un=subsidy$i.un,
                                                 subsidy.percentage = subsidy$subsidy.percentage,
                                                 period = p,
                                                 stringsAsFactors = F)
    )
    
  }
}

# COMBINE HARMFUL, TRADITIONAL AND SUBSIDY
data.percentages = merge(harmful.set[,c("name","harmful.percentage","period","i.un")],
                         merge(traditional.set[,c("name","traditional.percentage","period","i.un")],
                               subsidy.set[,c("name","subsidy.percentage","period","i.un")], by=c("name","period","i.un")), by=c("name","period","i.un"))

# SAVE FILE
save(data.percentages, file=paste0(data.path,"percentages.Rdata"))


#---------------------------------------------#
#                                             #
#   IMPORT AND EXPORT SHARES                  #
#                                             #
#---------------------------------------------#

# IMPORT SHARE
# Let "import share" refer to the share of imports covered by the new inward 
# red and amber measures implemented (of any type) by the jurisdiction 
# during the time period in question.

import.share <- data.frame()

for (i in 1:length(periods)) {
  for (g in 1:length(groups)) {
    print(paste0("Period: ",i))
    
    c.p=c(max(2009, year(periods[[i]][1])),year(periods[[i]][2]))
    
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       importers = groups[[g]],
                       keep.importers = T,
                       group.importers = F,
                       implementer.role = "importer",
                       coverage.period = c.p,
                       implementation.period = c(periods[[i]][1], periods[[i]][2]),
                       intra.year.duration = T) 
    
    trade.coverage.estimates$`Number of interventions affecting exported product` <- NULL
    trade.coverage.estimates$`Exporting country` <- NULL
    names(trade.coverage.estimates) <- c("name",c.p[1]:c.p[2])
    trade.coverage.estimates$import.share=rowMeans(trade.coverage.estimates[,c(2:ncol(trade.coverage.estimates))])
    trade.coverage.estimates=merge(trade.coverage.estimates, country.names[,c("name","un_code")],by="name", all.x=T)
    
    import.share <- rbind(import.share, data.frame(name = groups.name[g],
                                                   i.un=trade.coverage.estimates$un_code,
                                                   share = trade.coverage.estimates$import.share,
                                                   period = i,
                                                   stringsAsFactors = F))
  }
}


# EXPORT SHARE
# Let "export share" refer to own exports benefitting from the new red and amber 
# export promotion measures (MAST chapters P7 and P8) implemented by the jurisdiction during the time period in question.

## getting the HS codes
load("data/database replica/database replica - parts - base.Rdata")

gta_trade_value_bilateral(trade.data="2017")
total.exports=aggregate(trade.value ~ a.un, trade.base.bilateral,sum)
exports.by.hs <- aggregate(trade.value ~ a.un + hs6, trade.base.bilateral, sum)
setnames(exports.by.hs, "hs6", "affected.products")
data.table::setnames(total.exports, "a.un","un_code")

export.share <- data.frame()

for (i in 1:length(periods)) {
  for (g in 1:length(groups)) {
    print(paste0("Period: ",i))
    
    c.p=c(max(2009, year(periods[[i]][1])),year(periods[[i]][2]))
    
    # gta_trade_coverage(gta.evaluation=c("red","amber"),
    #                    affected.flows = "outward subsidy",
    #                    intervention.types = gtalibrary::int.mast.types$intervention.type[gtalibrary::int.mast.types$mast.subchapter.id %in% c("P7","P8")],
    #                    keep.type = T,
    #                    coverage.period = c.p,
    #                    implementation.period=c(periods[[i]][1], periods[[i]][2]),
    #                    trade.statistic = "share",
    #                    exporters = groups[[g]],
    #                    implementer.role = "3rd country",
    #                    keep.exporters = T,
    #                    group.exporters = F)
    
    gta_data_slicer(affected.flows = "outward subsidy",
                    gta.evaluation=c("red","amber"),
                    mast.chapters = export.promotion.measures,
                    keep.mast = T,
                    implementing.country = groups[[g]],
                    keep.implementer = T,
                    implementation.period = c(periods[[i]][1], periods[[i]][2]),
                    keep.implementation.na = F)
    
    os.by.a.un=unique(master.sliced[,c("intervention.id", "a.un")])
    
    # GET AFFECTED TARIFF LINES
    products=subset(gta_affected_tariff_line, intervention_id %in% os.by.a.un$intervention.id)[,c("intervention_id","affected_products","inception_date","removal_date")]
    
    names(products)=gsub("_","\\.", names(products))
    products$inception.date=as.Date(products$inception.date, "%Y-%m-%d")
    products$removal.date=as.Date(products$removal.date, "%Y-%m-%d")
    
    # SUBSET FOR PERIODS
    products=subset(products, (removal.date>=periods[[i]][2] | is.na(removal.date)) & (inception.date<=periods[[i]][1] | is.na(inception.date)) )
    
    # GET INTERVENTION DURATIONS
    gta_intervention_duration(years = c.p)
    intervention.duration <- subset(intervention.duration, intervention.id %in% os.by.a.un$intervention.id)
    
    # MERGE DURATION WITH PRODUCTS
    products <- merge(products, subset(intervention.duration, share > 0)[,c("intervention.id","share","year")], by="intervention.id")
    products <- aggregate(share~affected.products+year+intervention.id,products,max)
    # MERGE WITH EXPORT BY HS VALUES
    products <- merge(products, os.by.a.un, by="intervention.id")
    benefitting.exports <- merge(exports.by.hs, products[,c("affected.products","share","year","a.un")], by=c("affected.products","year","a.un"))
    
    # CALCULATE REAL AFFECTED VALUE PER HS AND AGGREGATE
    products$affected <- products$trade.value * products$share
    products <- aggregate(affected ~ a.un + year, products, sum)
    
    # MERGE WITH TOTAL TRADE DATA AND CALCULATE SHARE
    products <- merge(products, total.exports, by="a.un")
    products$share <- products$affected / products$trade.value
    
    
    #   export.share <- rbind(export.share, data.frame(name = groups.name[g],
    #                                                  share = trade.coverage.estimates$value,
    #                                                  period = paste0("period.",i),
    #                                                  year = trade.coverage.estimates$year))
  }
}



# SAVE FILE
# save(import.share, export.share, file=paste0(data.path,"shares.Rdata"))
save(import.share, file=paste0(data.path,"shares.Rdata"))

