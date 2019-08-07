rm(list=ls())

library(gtalibrary)
library(zoo)
library(tidyr)
library(data.table)

gta_setwd()
gta_data_slicer()

data.path='4 data queries/190807 Jens Hjarsbech Globalization Index/data/'


#### QUERY
# I want to construct the following series, but on a quarterly basis basis:
  
# The number of harmful and the number of liberalizing interventions globally and 
# affecting the EU and Denmark.
# I would need both the new interventions each new quarter but also 
# the number of interventions currently in force for each quarter from the beginning of your time series.



# with internal data ------------------------------------------------------
new.ids=unique(subset(master.sliced,!is.na(date.implemented) & date.implemented<=Sys.Date(),select=c('date.implemented','intervention.id')))
new.ids$qtr.impl=as.yearqtr(as.Date(new.ids$date.implemented,"%Y-%m-%d"))
new.ids$count=1
new.ids=aggregate(count~qtr.impl,new.ids,sum)

in.force.ids=unique(subset(master.sliced,!is.na(date.implemented),select=c('date.implemented','date.removed','intervention.id')))
in.force.ids$qtr.impl=as.yearqtr(as.Date(in.force.ids$date.implemented,"%Y-%m-%d"))
in.force.ids$qtr.rem=as.yearqtr(as.Date(in.force.ids$date.removed,"%Y-%m-%d"))
in.force.ids=subset(in.force.ids,select=c('qtr.impl','qtr.rem'))

#if implemented in a quarter, then it is considered in force in this quarter (<=)
#if removed during a quarter, then it is not considered in force this quarter (<)
for (qtr in paste0(rep(seq(2008,2019,1),each=4),' Q',1:4)){
  
  in.force.ids[[qtr]]=0
  in.force.ids[[qtr]][intersect(which(in.force.ids$qtr.impl<=as.yearqtr(qtr)),(which((in.force.ids$qtr.rem>as.yearqtr(qtr)) | (is.na(in.force.ids$qtr.rem)==T))))] = 1

}

in.force.ids=subset(in.force.ids,select=!(names(in.force.ids) %in% c('qtr.impl','qtr.rem')))
in.force.ids=data.frame(quarter=names(in.force.ids),
                        in.force.interventions=colSums(in.force.ids))
in.force.ids=in.force.ids[-nrow(in.force.ids),]

library(openxlsx)

setnames(new.ids,names(new.ids),c('Quarter','Number of new interventions implemented'))
setnames(in.force.ids,names(in.force.ids),c('Quarter','Total in force interventions'))

wb=createWorkbook()
sheet1='New Interventions'
sheet2='In Force Interventions'
addWorksheet(wb, sheet1)
addWorksheet(wb, sheet2)
writeData(wb, sheet1, new.ids)
writeData(wb, sheet2, in.force.ids)
saveWorkbook(wb,file=paste0(data.path,'New and in force interventions by quarter.xlsx'),overwrite=T)

