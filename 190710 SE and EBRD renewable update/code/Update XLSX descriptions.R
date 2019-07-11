rm(list=ls())

library(gtalibrary)
library(readxl)
library(plyr)
library(stringr)

gta_setwd()
query.path='4 data queries/190710 SE and EBRD renewable update/'
renewables.path=paste0(query.path,'data/Renewable_goods_HS_codes_extended.xlsx')
output.path=paste0(query.path,'results/Renewable_goods_HS_codes_extended.xlsx')
  
for (sheet in 1:length(excel_sheets(renewables.path))){
  sheet.names=tolower(gsub(' ','.',excel_sheets(renewables.path)))
  assign(sheet.names[sheet],xlsx::read.xlsx(renewables.path,sheetIndex = sheet,encoding = 'UTF-8'))
}

unique(nchar(overview$hs.2012))

## official descriptions
# Problem with incomplete labels by taking only the 6 level product description: 
# Certain labels are "For a voltage exceeding 1,000 V" or "Of an output exceeding 375 kVA", following workaround is proposed
official.descriptions=hs.2012.descriptions[hs.2012.descriptions$Product.level.6.ID %in% overview$hs.2012,]
incomplete.descriptions=str_detect(str_squish(as.character(official.descriptions$Product.level.6.description)),"^Of|^Having|^For")
official.descriptions$Product.level.4.description=str_squish(official.descriptions$Product.level.4.description)
official.descriptions$Product.level.6.description=str_squish(official.descriptions$Product.level.6.description)
labels=as.character(official.descriptions$Product.level.6.description)
labels[incomplete.descriptions]=paste(sub("[.]$","",as.character(official.descriptions$Product.level.4.description)[incomplete.descriptions]),
                                      as.character(official.descriptions$Product.level.6.description)[incomplete.descriptions])
labels=gsub('Of','of',labels)
labels=gsub('Having','having',labels)
labels=gsub('For','for',labels)
overview$official.description=labels

## APEC
overview$APEC.description[which(overview$APEC==1)]=mapvalues(overview$hs.2012[which(overview$APEC==1)],apec.list$HS..2012.,
                                                             as.character(apec.list$Description.of.the.product.in.the.original.source))
## WTO
wto.list=wto.list[wto.list$HS.code.2012 %in% overview$hs.2012,]
#two hs2012 mapped to two hs2002, will collapse-paste the descriptions
nrow(wto.list[duplicated(wto.list$HS.code.2012),])
wto.list=aggregate(Description.of.the.product.in.the.original.source~HS.code.2012,wto.list,function(x) paste(x,collapse = ' and '))
print(wto.list[wto.list$HS.code.2012%in%c(903020,903149),]$Description.of.the.product.in.the.original.source)
wto.list$Description.of.the.product.in.the.original.source=gsub('\n','',wto.list$Description.of.the.product.in.the.original.source)
overview$WTO.1[which(overview$WTO==1)]=mapvalues(overview$hs.2012[which(overview$WTO==1)],wto.list$HS.code.2012,
                                                 as.character(wto.list$Description.of.the.product.in.the.original.source))
overview$WTO.1[overview$hs.2012==overview$WTO.1]=NA

## ICTSD
ictsd.list=ictsd.list[ictsd.list$Appendix.B...Appendix.C..NO.DUPLICATE..HS.2012 %in% overview$hs.2012,]
#no duplicates in this case, no pasting necessary
nrow(ictsd.list[duplicated(ictsd.list$Appendix.B...Appendix.C..NO.DUPLICATE..HS.2012),])
ictsd.list$Description.of.the.product.in.the.original.source=gsub('^- ','',ictsd.list$Description.of.the.product.in.the.original.source)
overview$ICTSD.1[which(overview$ICTSD==1)]=mapvalues(overview$hs.2012[which(overview$ICTSD==1)],ictsd.list$Appendix.B...Appendix.C..NO.DUPLICATE..HS.2012,
                                                 as.character(ictsd.list$Description.of.the.product.in.the.original.source))
overview$ICTSD.1[overview$hs.2012==overview$ICTSD.1]=NA

data.table::setnames(overview,names(overview),c('hs 2012','APEC','ICTSD','WTO','official description','APEC description','ICTSD description','WTO description'))
# save --------------------------------------------------------------------
library(openxlsx)

file.copy(from = renewables.path, 
          to = output.path, 
          copy.mode = TRUE, 
          overwrite = TRUE)
wb <- openxlsx::loadWorkbook(output.path)
openxlsx::writeData(wb, sheet = "overview", overview)
openxlsx::saveWorkbook(wb,output.path,overwrite = T)


