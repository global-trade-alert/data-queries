rm(list=ls())
library(gtasql)
library(gtalibrary)
library(pool)
library(RMariaDB)
library(data.table)
library(gtabastiat)
library(xlsx)


# Settings
gta_setwd()
wd = paste0("4 data queries/201029 WB Food and medical coverages/")
output.path = paste0(wd, "output/")
data.path = paste0(wd, "data/")

# Open db connection
gta_sql_kill_connections()
database <- "gtamain"

gta_sql_pool_open(db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "gta_")


# Load codes
codes.food <- read.xlsx(paste0(data.path, "List_of_Products_-_102820.xlsx"),sheetIndex = 1)
codes.medical <- read.xlsx(paste0(data.path, "List_of_Products_-_102820.xlsx"),sheetIndex = 2)
# One code is 8-digit, changing here
codes.medical$HS.2017 <- as.numeric(substr(sprintf("%06s",as.character(codes.medical$HS.2017)), 1, 6))

# Create HS12 conversion dfs
# hs12conversion <- subset(gtalibrary::hs.vintages, origin.vintage == "HS 2017")[,c("origin.code","hs.2012")] # Does our hs.vintage table not include all codes?
# Loading conversion from csv
hs12conversion <- read.csv(file="definitions/hs-vintages/hs12-hs17.csv", sep = ";")
hs12conversion$hs17.4digit <- as.numeric(substr(sprintf("%06s",as.character(hs12conversion$hs17)), 1, 4))
hs12conversion$hs12.4digit <- as.numeric(substr(sprintf("%06s",as.character(hs12conversion$hs12)), 1, 4))
hs12.4digit <- hs12conversion[,c("hs17.4digit", "hs12.4digit")]
hs12.4digit <- unique(hs12.4digit)

# Convert food codes to HS12 and expand to 6-digit
codes.food <- merge(codes.food, hs12.4digit, by.x = "HS.2017...4.digit", by.y="hs17.4digit")
codes.food <- merge(codes.food, hs12conversion, by = "hs12.4digit")

# Convert medical codes to HS12
codes.medical <- merge(codes.medical, hs12conversion, by.x = "HS.2017", by.y = "hs17", all.x=T)
# 300210 seems to no longer exist in HS2017
codes.medical <- subset(codes.medical, is.na(hs12)==F)

# Retrieve interventions affecting these products in 2012

gta_data_slicer(gta.evaluation = c("Red","Amber"),
                hs.codes = unique(c(codes.food$hs12, codes.medical$hs12)),
                implementation.period = c("2020-01-01", "2020-12-31"),
                keep.implementation.na = F,
                keep.hs = T)


gta.inclusion=c("GTA - published")
wb.interventions <- unique(master.sliced$intervention.id)
wb.hs=unique(c(codes.food$hs12, codes.medical$hs12))

trade.coverage=gta_sql_get_value(paste0("SELECT DISTINCT gatl.intervention_id, tariff_line_code as hs6, gi.affected_flow_id, gj.un_code as i_un
                                                 FROM gta_affected_tariff_line gatl
                                                 JOIN gta_intervention gi
                                                 ON gatl.intervention_id = gi.id
                                                 JOIN gta_implementing_jurisdiction gij
                                                 ON gatl.intervention_id= gij.intervention_id
                                                 JOIN gta_jurisdiction gj
                                                 ON gij.jurisdiction_id = gj.id
                                                 WHERE gatl.intervention_id IN (",paste(wb.interventions, collapse=","),");"))

frozen.partners=gta_sql_get_value(paste0("SELECT DISTINCT gi.id as intervention_id, gj2.un_code as d_un, gj.un_code as a_un 
                                           FROM gta_intervention gi
                                           JOIN gta_distorted_market gdm
                                           ON gi.id= gdm.intervention_id
                                           JOIN gta_affected_jurisdiction gaj
                                           ON gi.id= gaj.intervention_id
                                           JOIN gta_jurisdiction gj
                                           ON gaj.jurisdiction_id = gj.id
                                           JOIN gta_jurisdiction gj2
                                           ON gdm.jurisdiction_id = gj2.id
                                           WHERE gi.id IN (",paste(wb.interventions, collapse=","),")
                                           AND (gi.dm_freeze=1 OR gi.aj_freeze=1)
                                           AND gaj.type!='D'
                                           AND gdm.type!='D';"))


trade.coverage$hs6=as.numeric(trade.coverage$hs6)
trade.coverage=subset(trade.coverage, hs6 %in% wb.hs)  

gta_trade_value_bilateral(hs.codes = wb.hs, trade.data = 2019)

coverage.imports=merge(subset(trade.coverage, affected.flow.id==1),
                       trade.base.bilateral,
                       by=c("i.un","hs6"), all.x=T)

setnames(trade.coverage, "i.un","t.un")
coverage.exports=merge(subset(trade.coverage, affected.flow.id==2),
                       trade.base.bilateral,
                       by.x=c("t.un","hs6"),by.y = c("a.un","hs6"), all.x=T)
setnames(coverage.exports, "i.un","a.un")
setnames(coverage.exports, "t.un","i.un")
setnames(trade.coverage, "t.un","i.un")

trade.coverage.abs=rbind(coverage.exports,
                         coverage.imports)

if(nrow(frozen.partners)>0){
  
  for(int in unique(frozen.partners$intervention.id)){
    
    trade.coverage.abs=rbind(subset(trade.coverage.abs, intervention.id!=int),
                             subset(trade.coverage.abs, intervention.id==int & a.un %in% frozen.partners$a.un[frozen.partners$intervention.id==int]))
  }
  
}


# Calculate trade shares for food and medical separately

#Add 4digit hs codes
trade.coverage.abs$hs4 <- as.numeric(substr(sprintf("%06s",as.character(trade.coverage.abs$hs6)), 1, 4))
trade.base.bilateral$hs4 <- as.numeric(substr(sprintf("%06s",as.character(trade.base.bilateral$hs6)), 1, 4))

# FOOD


# Metric 1: The absolute value of global exports or imports of the affected HS codes. To avoid confusion, this is 1 USD value per row.

trade.per.hs.food=aggregate(trade.value ~ hs4, unique(subset(trade.coverage.abs, hs6 %in% unique(codes.food$hs12))),sum)

# Metric 2: "What share of the affected goods world trade is implicated by the intervention in this row?"
trade.per.hs.food$global.share=0

for(hs in unique(subset(trade.per.hs.food, trade.value>0)$hs4)){
  
  trade.per.hs.food$global.share[trade.per.hs.food$hs4==hs]=round(trade.per.hs.food$trade.value[trade.per.hs.food$hs4==hs]/sum(trade.base.bilateral$trade.value[trade.base.bilateral$hs4==hs]),6)
  print(hs)
}

# MEDICAL
# Metric 1: The absolute value of global exports or imports of the affected HS codes. To avoid confusion, this is 1 USD value per row.
trade.per.hs.medical=aggregate(trade.value ~ hs6, unique(subset(trade.coverage.abs, hs6 %in% unique(codes.medical$hs12))),sum)

# Metric 2: "What share of the affected goods world trade is implicated by the intervention in this row?"
trade.per.hs.medical$global.share=0

for(hs in unique(subset(trade.per.hs.medical, trade.value>0)$hs6)){
  
  trade.per.hs.medical$global.share[trade.per.hs.medical$hs6==hs]=round(trade.per.hs.medical$trade.value[trade.per.hs.medical$hs6==hs]/sum(trade.base.bilateral$trade.value[trade.base.bilateral$hs6==hs]),6)
  print(int)
}


# Prettify results

# FOOD
final.food <- merge(codes.food, trade.per.hs.food, by.x = "hs12.4digit", by.y = "hs4")

# Check if any hs17 differs from hs12
sum(final.food$hs12.4digit - final.food$HS.2017...4.digit) > 0 # SHOULD BE FALSE

final.food <- unique(final.food[,c("hs17.4digit", "Group","Product.Description","global.share")])

# MEDICAL
final.food <- merge(codes.food, trade.per.hs.food, by.x = "hs12.4digit", by.y = "hs4")

# Check if any hs17 differs from hs12
sum(final.food$hs12.4digit - final.food$HS.2017...4.digit) > 0 # SHOULD BE FALSE

final.food <- unique(final.food[,c("hs17.4digit", "Group","Product.Description","global.share")])

