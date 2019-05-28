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
harmful.set <- data.frame(name = character(),
                      harmful.percentage = numeric(),
                      period = character(),
                      year = numeric())

traditional.set <- data.frame(name = character(),
                      traditional.percentage = numeric(),
                      period = character(),
                      year = numeric())

subsidy.set <- data.frame(name = character(),
                      subsidy.percentage = numeric(),
                      period = character(),
                      year = numeric())

for (p in 1:length(periods)) {
  
  for(g in 1:length(groups)) {
  
    print(paste0("Period: ",p))
  
    # HARMFUL 
    # Let "% harmful" refer to the total number of red or amber implemented measures 
    # as a % of the total number of implemented measures by a jurisdiction.
    
    gta_data_slicer(implementation.period = c(periods[[p]][1], periods[[p]][2]),
                    keep.implementation.na = F,
                    implementing.country = groups[[g]])
    harmful <- master.sliced
    
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
                                                 period = paste0("period.",p)
                                                 )
                         )
    
    
    # TRADITIONAL
    # Let "% traditional" refer to the total number of red and amber implemented 
    # tariff increases, MAST chapter D, and MAST subchapters E1, E2, E5, E6, and E9 
    # as a % of all red and amber implemented measures by a jurisdiction.
    
    traditional <- subset(master.sliced, i.un %in% groups[[g]])
    
    # SET MAST CHAPTER TO MAST AND OTHER
    traditional$mast.chapter <- as.character(traditional$mast.chapter)
    traditional$mast <- "Other"
    traditional$mast[traditional$mast.chapter %in% c("D","TARIFF") | traditional$mast.id %in% c("E1", "E2", "E5", "E6", "E9")] <- "MAST"
    
    
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
                                                         period = paste0("period.",p)))
    
    # SUBSIDY
    # Let "% subsidy" refer to the total number of red and amber implemented MAST 
    # chapter L, P7 and P8 measures as a % of all red and amber implemented 
    # measures by a jurisdiction.
     subsidy <-  subset(master.sliced, i.un %in% groups[[g]])
    
    # SET MAST CHAPTER TO MAST AND OTHER
    subsidy$mast.chapter <- as.character(subsidy$mast.chapter)
    subsidy$mast <- "Other"
    subsidy$mast[subsidy$mast.chapter %in% c("L") | subsidy$mast.id %in% c("P7", "P8")] <- "MAST"
  
    
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
                                                 period = paste0("period.",p)
                                                 )
                         )
    
  }
}

# COMBINE HARMFUL, TRADITIONAL AND SUBSIDY
data.percentages = merge(harmful.set[,c("name","harmful.percentage","period","year")],
                         merge(traditional.set[,c("name","traditional.percentage","period","year")],
                                subsidy.set[,c("name","subsidy.percentage","period","year")], by=c("name","period","year")), by=c("name","period","year"))

# CONVERT TO CHARACTER
data.percentages$name <- as.character(data.percentages$name)
data.percentages$period <- as.character(data.percentages$period)

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

import.share <- data.frame(name = character(),
                           share = numeric(),
                           period = character(),
                           year = numeric())

for (i in 1:length(periods)) {
  for (g in 1:length(groups)) {
    print(paste0("Period: ",i))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       importers = groups[[g]],
                       keep.importers = T,
                       group.importers = T,
                       implementer.role = "importer",
                       coverage.period = c(year(periods[[i]][1]),year(periods[[i]][2])),
                       implementation.period = c(periods[[i]][1], periods[[i]][2]),
                       intra.year.duration = T) # INTRA YEAR DURATION YES OR NO?
    
    trade.coverage.estimates$`Number of interventions affecting exported product` <- NULL
    trade.coverage.estimates$`Exporting country` <- NULL
    names(trade.coverage.estimates) <- c("name",year(periods[[i]][1]):year(periods[[i]][2]))
    trade.coverage.estimates <- gather(trade.coverage.estimates, year, value, 2:ncol(trade.coverage.estimates))
    
    import.share <- rbind(import.share, data.frame(name = groups.name[g],
                                                   share = trade.coverage.estimates$value,
                                                   period = paste0("period.",i),
                                                   year = trade.coverage.estimates$year))
  }
}
  

# EXPORT SHARE
# Let "export share" refer to the exports covered by the new red and amber 
# measures implemented (of any type) by the jurisdiction during the time period in question.

export.share <- data.frame(name = character(),
                           share = numeric(),
                           period = character())

for (i in 1:length(periods)) {
  for (g in 1:length(groups)) {
    print(paste0("Period: ",i))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       affected.flows = "outward",
                       exporters = groups[[g]],
                       keep.exporters = T,
                       group.exporters = T,
                       implementer.role = "exporter",
                       coverage.period = c(year(periods[[i]][1]),year(periods[[i]][2])),
                       implementation.period = c(periods[[i]][1], periods[[i]][2]),
                       intra.year.duration = T) # INTRA YEAR DURATION YES OR NO?
    
    trade.coverage.estimates$`Number of interventions affecting exported product` <- NULL
    trade.coverage.estimates$`Importing country` <- NULL
    names(trade.coverage.estimates) <- c("name",year(periods[[i]][1]):year(periods[[i]][2]))
    trade.coverage.estimates <- gather(trade.coverage.estimates, year, value, 2:ncol(trade.coverage.estimates))
    
    export.share <- rbind(export.share, data.frame(name = groups.name[g],
                                                   share = trade.coverage.estimates$value,
                                                   period = paste0("period.",i),
                                                   year = trade.coverage.estimates$year))
  }
}


# SAVE FILE
save(import.share, export.share, file=paste0(data.path,"shares.Rdata"))