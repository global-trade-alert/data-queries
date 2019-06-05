rm(list=ls())

library(gtalibrary)
gta_setwd()

project = '4 data queries'
query = '190604 Simon Panel US-Sino tariff war'
output.path = paste(project, query,'output/',sep='/')

gta_data_slicer()

master.sliced = subset(master.sliced, i.un == 840 & a.un == 156 & mast.chapter == 'L' & !is.na(date.implemented) & gta.evaluation %in% c('Red','Amber'))
master.sliced = subset(cSplit(master.sliced,which(colnames(master.sliced)=='affected.sector'), direction = "long", sep = ",",drop=TRUE), affected.sector < 500)

master.sliced = aggregate(intervention.id ~ intervention.type + implementation.level, master.sliced, function(x) length(unique(x)))
master.sliced = rbind(master.sliced, data.frame(intervention.type = 'Any', implementation.level = 'Any', intervention.id = sum(master.sliced$intervention.id)))
names(master.sliced) = c('Type of Subsidy', 'Implementation level', 'Number of Subsidy interventions implemented (but not necessarily in force)')


xlsx::write.xlsx(master.sliced, file = paste0(output.path, 'Subsidy breakdown by type and implementation level.xlsx'), sheetName='Sheet 1', row.names = F)
