rm(list=ls())

library(gtalibrary)
library(data.table)
library(tidyr)

gta_setwd()

query.path='4 data queries/190710 SE and EBRD renewable update/'
output.path=paste0(query.path,'results/Renewable_goods_HS_codes_extended.xlsx')

overview=xlsx::read.xlsx(output.path,sheetName = 'overview')
original.colnames=names(overview)
trade.data.path="data/support tables/Goods support table for gtalibrary.Rdata"
load(trade.data.path)
trade.base=trade.annual

cty=subset(country.names, is.ebrd==T)$un_code
trade.base=subset(trade.annual, year%in% 2015:2017)
trade.base=subset(trade.base,hs6 %in% overview$hs.2012)
importing.base=trade.base
exporting.base=trade.base

importing.base=subset(importing.base,i.un %in% cty)
importing.base=aggregate(trade.value~i.un+hs6,importing.base,sum)
importing.base=merge(expand.grid(i.un=cty,hs6=overview$hs.2012),importing.base,by=c('hs6','i.un'),all.x = T)
importing.base$trade.value[is.na(importing.base$trade.value)]=0

exporting.base=subset(exporting.base,a.un %in% cty)
exporting.base=aggregate(trade.value~a.un+hs6,exporting.base,sum)
exporting.base=merge(expand.grid(a.un=cty,hs6=overview$hs.2012),exporting.base,by=c('hs6','a.un'),all.x = T)
exporting.base$trade.value[is.na(exporting.base$trade.value)]=0

# importer stats ----------------------------------------------------------
#max
temp=merge(aggregate(trade.value~hs6,importing.base,max),importing.base,by=c('trade.value','hs6'))
setnames(temp,names(temp),c('importer.max.value','hs.2012','importer.max.name'))
overview=merge(overview,temp,by='hs.2012',all.x=T)
overview$importer.max.name=plyr::mapvalues(overview$importer.max.name,country.names$un_code,as.character(country.names$name))
overview=overview[!duplicated(overview$hs.2012),]

#mean
temp=aggregate(trade.value~hs6,importing.base,mean)
overview$importer.mean=plyr::mapvalues(overview$hs.2012,temp$hs6,temp$trade.value)

#percentage of ebrd countries under 5% of mean (somewhat of inequality index), i'm sorry if this was a dumb idea
temp=merge(temp, importing.base,by=c('hs6'),all.y = T)
temp$perc.mean=temp$trade.value.y/temp$trade.value.x
temp$perc.mean[is.na(temp$perc.mean)]=0
temp$less.than.5perc.mean=0
temp$less.than.5perc.mean[temp$perc.mean<0.01]=1
temp=aggregate(less.than.5perc.mean~hs6,temp,sum)
overview$nb.imp.sub.5perc.mean=plyr::mapvalues(overview$hs.2012,temp$hs6,temp$less.than.5perc.mean)
overview$nb.imp.sub.5perc.mean[overview$importer.mean==0]=NA

#median
temp=aggregate(trade.value~hs6,importing.base,median)
overview$importer.median=plyr::mapvalues(overview$hs.2012,temp$hs6,temp$trade.value)

#nonzero values
temp=importing.base
temp$imp.non.zero=0
temp$imp.non.zero[temp$trade.value>0]=1
temp=aggregate(imp.non.zero~hs6,temp,sum)
overview$imp.non.zero=plyr::mapvalues(overview$hs.2012,temp$hs6,temp$imp.non.zero)

setnames(overview,names(overview)[!names(overview)%in%original.colnames],
                                  c('Maximum value imported by single country',
                                    'Largest importer',
                                    'Mean imported value',
                                    'Number of importers importing less than 5% of mean value',
                                    'Median imported value',
                                    'Number of non-zero value imported importers'))
# export stats ------------------------------------------------------------

#max through all countries
temp=merge(aggregate(trade.value~hs6,exporting.base,max),exporting.base,by=c('trade.value','hs6'))
setnames(temp,names(temp),c('exporter.max.value','hs.2012','exporter.max.name'))
overview=merge(overview,temp,by='hs.2012',all.x=T)
overview$exporter.max.name=plyr::mapvalues(overview$exporter.max.name,country.names$un_code,as.character(country.names$name))
overview=overview[!duplicated(overview$hs.2012),]

setnames(overview,c('exporter.max.value','exporter.max.name'),c('Maximum value exported by single country','Largest exporter'))
#max per country
exporting.base$a.un=plyr::mapvalues(exporting.base$a.un,country.names$un_code,paste(as.character(country.names$name),'value exported'))
overview=merge(overview,spread(exporting.base,a.un,trade.value),by.x='hs.2012',by.y='hs6')


# save --------------------------------------------------------------------
setnames(overview,original.colnames,c('hs 2012','APEC','ICTSD','WTO','official description','APEC description','ICTSD description','WTO description'))

library(openxlsx)

wb <- openxlsx::loadWorkbook(output.path)
openxlsx::writeData(wb, sheet = "overview", overview)
openxlsx::saveWorkbook(wb,output.path,overwrite = T)
