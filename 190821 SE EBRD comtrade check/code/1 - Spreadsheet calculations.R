# rm(list=ls())

library(gtalibrary)
library(readxl)
library(plyr)
library(readr)
library(data.table)
library(tidyr)

## request
## SE:
# Johannes is going to send you the original UN COMTRADE data for 2015-2017.
# Please also recalculate the attached spreadsheets using the original COMTRADE data.
# Don't change the thresholds used in each file ($50m and $25m, as appropriate.)

# SE told me to replicate the attached excel files:

# For the Competitively produced RE goods excel file, calculate the minimum exports/imports for each country in a year within 2015-2017
# If the minimum is over 50 million $ then color green, mark as red if not
# For the Tradeability goods in EBRD COO excel file, count, again over 2015-2017, the number of countries that cleared certain thresholds (given by SE), again for both the import and export side
# If imports were over 50m, then the good was considered tradeable and marked by a 'Yes'. If goods were not over 50m, then it was marked with a 'no'. 
# If imports were untradeable at 50m threshold, but tradeable at 25m threshold, then tradeability changes with a lower threshold was marked as 'Yes'. 
# Do all of this for the EU15 and the EBRD separately

gta_setwd()
data.path='4 data queries/190821 SE EBRD comtrade check/data/'
output.path='4 data queries/190821 SE EBRD comtrade check/output/'

cty.ebrd=subset(country.names,is.ebrd==T)$un_code

check.ebrd=read_excel(paste0(data.path,"Tradeability of RE goods in EBRD COO robustness check1.xlsx"),skip = 1)

check.eu15=read_excel(paste0(data.path,"Tradeability of RE goods in EBRD COO robustness check1.xlsx"),skip = 1)

cty.eu15=read_excel(paste0(data.path,"Tradeability of RE goods in EBRD COO robustness check2.xlsx"),sheet = 3)$EU15
cty.eu15=mapvalues(cty.eu15,country.names$name,country.names$un_code)

ebrd.by.cty=read_excel(paste0(data.path,"Competitively produced RE goods in EBRD COO.xlsx"),sheet = 1,skip=1)

hs.codes=unique(c(as.numeric(check.eu15$`HS 2012 code`),as.numeric(check.ebrd$`HS 2012 code`),as.numeric(ebrd.by.cty$`HS 2012 code`)))
hs.codes=hs.codes[!is.na(hs.codes)]
# create trade base -------------------------------------------------------------------------

trade.2015=read_csv(paste0(data.path,'2015 - HS as reported - AG6 - ALL ALL - import & export - no threshold.zip'))
trade.2016.2017=read_csv(paste0(data.path,'2016-2017 - HS as reported - AG6 - ALL ALL - import - no threshold.zip'))
names(trade.2015)=gsub(' ','.',names(trade.2015))
names(trade.2016.2017)=gsub(' ','.',names(trade.2016.2017))

trade.2015=subset(trade.2015, Trade.Flow.Description=='Import' & (Reporter.Code %in% c(cty.ebrd,cty.eu15) | Partner.Code %in% c(cty.ebrd,cty.eu15)) ,select=c('Reporter.Code','Partner.Code','Commodity.Code','Value','Year'))
trade.2016.2017=subset(trade.2016.2017, Trade.Flow.Description=='Import' & (Reporter.Code %in% c(cty.ebrd,cty.eu15) | Partner.Code %in% c(cty.ebrd,cty.eu15)),select=c('Reporter.Code','Partner.Code','Commodity.Code','Value','Year'))

trade=rbind(trade.2015,trade.2016.2017)
rm(trade.2015,trade.2016.2017)

trade$Commodity.Code=as.numeric(gsub('\\D','',trade$Commodity.Code))
trade=subset(trade, Commodity.Code %in% hs.codes)

setnames(trade, names(trade),c('i.un','a.un','hs6','trade.value','year'))
save(trade, file=paste0(data.path,'trade ebrd eu 15.Rdata'))

# calculate min exports and imports in individual year over 2015-2017 --------------------------------------------
# For the Competitively produced RE goods excel file, calculate the minimum exports/imports for each country in a year within 2015-2017
# If the minimum is over 50 million $ then color green, mark as red if not
# do this for EU15 and EBRD separately 
load(paste0(data.path,'trade ebrd eu 15.Rdata'))

tsh=50000000

# Removing a.un = 0. That's world
trade=subset(trade, a.un>0)


exporting.base=aggregate(trade.value~a.un+hs6+year,trade,sum)
exp.min.base=data.frame(hs=hs.codes)

#fill missing rows (to allow aggregate by min to find 0 values)
exporting.base=merge(expand.grid(a.un=c(cty.ebrd,cty.eu15),hs6=hs.codes,year=2015:2017),exporting.base,by=c('hs6','a.un','year'),all.x = T)
exporting.base$trade.value[is.na(exporting.base$trade.value)]=0

#exporters
temp=aggregate(trade.value~a.un+hs6,exporting.base,min)
exp.min.base=merge(exp.min.base,temp, by.x='hs',by.y='hs6',all.y=T)
setnames(exp.min.base,'trade.value',paste0('exp more than ',tsh))
exp.min.base$a.un=plyr::mapvalues(exp.min.base$a.un,as.character(country.names$un_code),as.character(country.names$name))
exp.min.base=spread(exp.min.base, a.un,paste0('exp more than ',tsh))
exp.min.base[is.na(exp.min.base)]=0

##importers
importing.base=aggregate(trade.value~i.un+hs6+year,trade,sum)
imp.min.base=data.frame(hs=hs.codes)

importing.base=merge(expand.grid(i.un=c(cty.ebrd,cty.eu15),hs6=hs.codes,year=2015:2017),importing.base,by=c('hs6','i.un','year'),all.x = T)
importing.base$trade.value[is.na(importing.base$trade.value)]=0

temp=aggregate(trade.value~i.un+hs6,importing.base,min)
imp.min.base=merge(imp.min.base,temp, by.x='hs',by.y='hs6',all.y=T)
setnames(imp.min.base,'trade.value',paste0('exp more than ',tsh))
imp.min.base$i.un=plyr::mapvalues(imp.min.base$i.un,as.character(country.names$un_code),as.character(country.names$name))
imp.min.base=spread(imp.min.base, i.un,paste0('exp more than ',tsh))
imp.min.base[is.na(imp.min.base)]=0

## output 
library(openxlsx)

#rearrange order to match xlsx order
exp.min.base=exp.min.base[match(hs.codes,exp.min.base$hs),]
imp.min.base=imp.min.base[match(hs.codes,imp.min.base$hs),]

groups=list(EU15=cty.eu15,
            EBRD=cty.ebrd)

for (grp in 1:length(groups)){
  grp.name=names(groups)[grp] 
  grp.cty=plyr::mapvalues(groups[[grp]],country.names$un_code,country.names$name)
   
  wb=loadWorkbook(paste0(data.path,'Competitively produced RE goods in EBRD COO.xlsx'))
  writeData(wb, 'export',rowNames=F,colNames=F,startCol=9,startRow=3,x=subset(exp.min.base, select=grp.cty))
  writeData(wb, 'import',rowNames=F,colNames=F,startCol=9,startRow=3,x=subset(imp.min.base, select=grp.cty))
  
  negStyle <- createStyle(bgFill = "#DC143C")
  posStyle <- createStyle(bgFill = "#008000")
  conditionalFormatting(wb, "export", cols=9:45, rows=3:89, rule="<50000000", style = negStyle)
  conditionalFormatting(wb, "export", cols=9:45, rows=3:89, rule=">50000000", style = posStyle)
  conditionalFormatting(wb, "import", cols=9:45, rows=3:89, rule="<50000000", style = negStyle)
  conditionalFormatting(wb, "import", cols=9:45, rows=3:89, rule=">50000000", style = posStyle)
  setColWidths(wb, "export", cols = 9:45, widths = 12)
  setColWidths(wb, "import", cols = 9:45, widths = 12)
  saveWorkbook(wb, file=paste0(output.path,grp.name,' - Original UN Comtrade - Competitively produced RE goods in EBRD COO.xlsx'),overwrite=T)
  
}  


# count how many over thresholds ------------------------------------------
# For the Tradeability goods in EBRD COO excel file, count, again over 2015-2017, the number of countries that cleared certain thresholds (25m, 50m, 75m, 100m), again for both the import and export side
# If imports were over 50m, then the good was considered tradeable and marked by a 'Yes'. If goods were not over 50m, then it was marked with a 'no'. 
# If imports were untradeable at 50m threshold, but tradeable at 25m threshold, then tradeability changes with a lower threshold was marked as 'Yes'. 
# do this for EU15 and EBRD separately 

# more elegant would have been to paste the un codes over the thresholds and do the counting at the end to avoid 4 different tables
# with the time-frame I went with the first option that came to mind, hoping to finish it wednesday night, is it worthwhile i rewrite this?

eu.exp.min.base=subset(exp.min.base, select=c('hs',plyr::mapvalues(cty.eu15,country.names$un_code,country.names$name)))
ebrd.exp.min.base=subset(exp.min.base, select=c('hs',plyr::mapvalues(cty.ebrd,country.names$un_code,country.names$name)))

eu.imp.min.base=subset(imp.min.base, select=c('hs',plyr::mapvalues(cty.eu15,country.names$un_code,country.names$name)))
ebrd.imp.min.base=subset(imp.min.base, select=c('hs',plyr::mapvalues(cty.ebrd,country.names$un_code,country.names$name)))

thresholds=c(25000000,50000000,75000000,100000000)

for (tsh in thresholds){
  
  eu.exp.min.base[[paste0('exp.tsh.',as.character(tsh))]]=NA
  ebrd.exp.min.base[[paste0('exp.tsh.',as.character(tsh))]]=NA
  eu.imp.min.base[[paste0('imp.tsh.',as.character(tsh))]]=NA
  ebrd.imp.min.base[[paste0('imp.tsh.',as.character(tsh))]]=NA
    
  for (i in 1:nrow(exp.min.base)){
    
    #Count across rows how many are over the threshold, and fill column in
    eu.exp.min.base[[paste0('exp.tsh.',as.character(tsh))]][i]=length(which(eu.exp.min.base[i,-1]>tsh))
    ebrd.exp.min.base[[paste0('exp.tsh.',as.character(tsh))]][i]=length(which(ebrd.exp.min.base[i,-1]>tsh))
    eu.imp.min.base[[paste0('imp.tsh.',as.character(tsh))]][i]=length(which(eu.imp.min.base[i,-1]>tsh))
    ebrd.imp.min.base[[paste0('imp.tsh.',as.character(tsh))]][i]=length(which(ebrd.imp.min.base[i,-1]>tsh))

  }  
}  

eu.exp.min.base=subset(eu.exp.min.base, 
                       select=names(eu.exp.min.base)[!names(eu.exp.min.base) %in% plyr::mapvalues(cty.eu15,country.names$un_code,country.names$name)])
ebrd.exp.min.base=subset(ebrd.exp.min.base, 
                       select=names(ebrd.exp.min.base)[!names(ebrd.exp.min.base) %in% plyr::mapvalues(cty.ebrd,country.names$un_code,country.names$name)])
eu.imp.min.base=subset(eu.imp.min.base, 
                       select=names(eu.imp.min.base)[!names(eu.imp.min.base) %in% plyr::mapvalues(cty.eu15,country.names$un_code,country.names$name)])
ebrd.imp.min.base=subset(ebrd.imp.min.base, 
                       select=names(ebrd.imp.min.base)[!names(ebrd.imp.min.base) %in% plyr::mapvalues(cty.ebrd,country.names$un_code,country.names$name)])


#merge exp and imp values
ebrd=merge(ebrd.imp.min.base, ebrd.exp.min.base, by='hs')
eu=merge(eu.imp.min.base, eu.exp.min.base, by='hs')

#If >0 countries importing more than 50m then tradeable at 50m 
#If non tradeable at 50m but tradeable at 25, then tradeability changes = yes
ebrd$tradeable.50m='No'
ebrd$tradeable.50m[which(ebrd$`imp.tsh.5e+07`>0)]='Yes'
ebrd$tradeable.25m='No'
ebrd$tradeable.25m[which(ebrd$`imp.tsh.2.5e+07`>0)]='Yes'
ebrd$tradeability.changes=NA
ebrd$tradeability.changes[which(ebrd$tradeable.50m=='No' & ebrd$tradeable.25m=='Yes')]='Yes'
ebrd$tradeable.25m=NULL

eu$tradeable.50m='No'
eu$tradeable.50m[which(eu$`imp.tsh.5e+07`>0)]='Yes'
eu$tradeable.25m='No'
eu$tradeable.25m[which(eu$`imp.tsh.2.5e+07`>0)]='Yes'
eu$tradeability.changes=NA
eu$tradeability.changes[which(eu$tradeable.50m=='No' & eu$tradeable.25m=='Yes')]='Yes'
eu$tradeable.25m=NULL

## ebrd output
library(openxlsx)

#rearrange order to match xlsx order
ebrd=ebrd[match(hs.codes,ebrd$hs),]

wb=loadWorkbook(paste0(data.path,'Tradeability of RE goods in EBRD COO robustness check1.xlsx'))
writeData(wb, 'HS codes and trade data',rowNames=F,colNames=F,startCol=1,startRow=3,x=subset(ebrd, select=c('tradeable.50m','tradeability.changes')))
writeData(wb, 'HS codes and trade data',rowNames=F,colNames=F,startCol=12,startRow=3,x=subset(ebrd, select=names(ebrd)[!names(ebrd) %in% c('hs','tradeable.50m','tradeability.changes')]))
writeData(wb, 'Sheet1',rowNames=F,colNames=F,startCol=1,startRow=3,x=subset(ebrd, select=c('tradeable.50m','tradeability.changes')))

##REMEMBER TO DELETE ROWS 91 IN FIRST SHEET AND 91-94 IN SECOND SHEET
          
saveWorkbook(wb, file=paste0(output.path,'EBRD - Original UN Comtrade - Tradeability of RE goods in EBRD COO.xlsx'),overwrite=T)
  
## eu15 output  
eu=eu[match(hs.codes,eu$hs),]

wb=loadWorkbook(paste0(data.path,'Tradeability of RE goods in EBRD COO robustness check2.xlsx'))

writeData(wb, 'EU15 results',rowNames=F,colNames=F,startCol=1,startRow=3,x=subset(eu, select=c('tradeable.50m','tradeability.changes')))
writeData(wb, 'EU15 results',rowNames=F,colNames=F,startCol=11,startRow=3,x=subset(eu, select=names(eu)[!names(eu) %in% c('hs','tradeable.50m','tradeability.changes')]))

##REMEMBER TO DELETE ROW 90 in first SHEET
##ALSO DELETE SECOND SHEET
saveWorkbook(wb, file=paste0(output.path,'EU15 - Original UN Comtrade - Tradeability of RE goods in EBRD COO.xlsx'),overwrite=T)