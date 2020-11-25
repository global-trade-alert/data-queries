# The chart we want to provide shall be similar to the chart on your website https://www.globaltradealert.org/ at the top on the left-hand side.But potentially we want to zoom into some time periods. Therefore, monthly data would be great.
# We would then need to know the number of trade interventions per type (green / red / amber) for each month (MM-YYYY).
# Regarding the month of the intervention, we are not yet sure whether to use the announcement date or the inception date. We would like to stick to your methodology. Do you rather use the announcement date or the inception date when showing the number of new interventions?


library(gtalibrary)
library(data.table)
library(xlsx)
gta_setwd()

project.path="4 data queries/201125 UNIDO"

gta_data_slicer(lag.adjustment = "11-25",
                implementation.period = c("2008-11-01","2020-11-25"),
                keep.implementation.na = F)
unido.lag=aggregate(intervention.id ~ gta.evaluation + year(date.implemented)  + month(date.implemented),master.sliced, function(x) length(unique(x)))
names(unido.lag)=c("GTA evaluation","Year implemented","Month implemented","Number of interventions worldwide")
write.xlsx(unido.lag, file=paste0(project.path,"/result/GTA data for UNIDO.xlsx"), sheetName = "lag-adjusted", row.names = F)


gta_data_slicer(implementation.period = c("2008-11-01","2020-11-25"),
                keep.implementation.na = F)
unido.full=aggregate(intervention.id ~ gta.evaluation + year(date.implemented) + month(date.implemented),master.sliced, function(x) length(unique(x)))
names(unido.full)=c("GTA evaluation","Year implemented","Month implemented","Number of interventions worldwide")
write.xlsx(unido.full, file=paste0(project.path,"/result/GTA data for UNIDO.xlsx"), sheetName = "full database", append=T, row.names = F)
