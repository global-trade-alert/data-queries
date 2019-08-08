rm(list=ls())

library(gtalibrary)
library(zoo)
library(tidyr)
library(data.table)

gta_setwd()
data.path='4 data queries/190807 Jens Hjarsbech Globalization Index/data/'

#### QUERY
# I want to construct the following series, but on a quarterly basis basis:

# The number of harmful and the number of liberalizing interventions globally and 
# affecting the EU and Denmark.
# I would need both the new interventions each new quarter but also 
# the number of interventions currently in force for each quarter from the beginning of your time series.

gta_data_slicer()
base.data=subset(master.sliced, a.un %in% subset(country.names,is.eu==T)$un_code)
base.data=unique(subset(base.data,!is.na(date.implemented) & date.implemented<=Sys.Date() & gta.evaluation %in% c('Red','Green'), select=c('date.implemented','date.removed','intervention.id','gta.evaluation','affected.jurisdiction')))

eu.base=unique(subset(base.data,select=c('date.implemented','date.removed','intervention.id','gta.evaluation')))
dk.base=unique(subset(base.data,affected.jurisdiction=='Denmark',select=c('date.implemented','date.removed','intervention.id','gta.evaluation')))

# eu ----------------------------------------------------------------------
for(base in 1:length(list(eu.base,dk.base))){
  data=list(eu.base,dk.base)[[base]]
  base.name=c('eu','dk')[[base]]
  
  new.ids=data
  in.force.ids=data
  
  new.ids=unique(subset(new.ids,select=c('date.implemented','intervention.id','gta.evaluation')))
  new.ids$qtr.impl=as.yearqtr(as.Date(new.ids$date.implemented,"%Y-%m-%d"))
  new.ids=aggregate(intervention.id~qtr.impl+gta.evaluation,new.ids,function(x) length(unique(x)))
  new.ids=spread(new.ids,gta.evaluation,intervention.id)
  new.ids=merge(data.frame(qtr.impl=as.yearqtr(paste0(rep(seq(2008,2019,1),each=4),' Q',1:4))),new.ids,by='qtr.impl',all.x=T)
  new.ids=new.ids[-nrow(new.ids),]
  new.ids[is.na(new.ids)]=0
  
  setnames(new.ids,names(new.ids),c('Quarter','Number of new liberalising interventions implemented','Number of new harmful interventions implemented'))
  
  in.force.ids$qtr.impl=as.yearqtr(as.Date(in.force.ids$date.implemented,"%Y-%m-%d"))
  in.force.ids$qtr.rem=as.yearqtr(as.Date(in.force.ids$date.removed,"%Y-%m-%d"))
  in.force.ids=subset(in.force.ids,select=c('qtr.impl','qtr.rem','gta.evaluation'))
  
  #if implemented in a quarter, then it is considered in force in this quarter (<=)
  #if removed during a quarter, then it is not considered in force this quarter (<)
  for (qtr in paste0(rep(seq(2008,2019,1),each=4),' Q',1:4)[1]){
    
    in.force.ids[[qtr]]=0
    in.force.ids[[qtr]][intersect(which(in.force.ids$qtr.impl<=as.yearqtr(qtr)),(which((in.force.ids$qtr.rem>as.yearqtr(qtr)) | (is.na(in.force.ids$qtr.rem)==T))))] = 1
    
  }
  
  red.in.force.ids=subset(in.force.ids,gta.evaluation=='Red',select=!(names(in.force.ids) %in% c('qtr.impl','qtr.rem','gta.evaluation')))
  red.in.force.ids=data.frame(quarter=names(red.in.force.ids),
                          in.force.interventions=colSums(red.in.force.ids))
  
  green.in.force.ids=subset(in.force.ids,gta.evaluation=='Green',select=!(names(in.force.ids) %in% c('qtr.impl','qtr.rem','gta.evaluation')))
  green.in.force.ids=data.frame(quarter=names(green.in.force.ids),
                              in.force.interventions=colSums(green.in.force.ids))

  in.force=merge(green.in.force.ids,red.in.force.ids,by='quarter')
  in.force=in.force[-nrow(in.force),]
  
  setnames(in.force,names(in.force),c('Quarter','Total in force liberalising interventions','Total in force harmful interventions'))
  
  assign(paste0(base.name,'.in.force'),in.force)
  assign(paste0(base.name,'.new.ids'),new.ids)
  
  
}

library(openxlsx)

wb=createWorkbook()
sheet1='EU aff. new int.'
sheet2='EU aff. in force int.'
addWorksheet(wb, sheet1)
addWorksheet(wb, sheet2)
writeData(wb, sheet1, eu.new.ids)
writeData(wb, sheet2, eu.in.force)
sheet3='DK aff. new int.'
sheet4='DK aff. in force int.'
addWorksheet(wb, sheet3)
addWorksheet(wb, sheet4)
writeData(wb, sheet3, dk.new.ids)
writeData(wb, sheet4, dk.in.force)
saveWorkbook(wb,file=paste0(data.path,'New and in force interventions by quarter.xlsx'),overwrite=T)