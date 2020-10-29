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
names(codes.food)=c("group","hs","description")

hs.food=as.numeric(gta_hs_code_check(codes.food$hs))

## did this via a google Sheet. Function did not work proper.
hs.med=c(190110, 210610, 210690, 220710, 220890, 284700, 290512, 293621, 293622, 293623, 293624, 293625, 293626, 293627, 293628, 293629, 293690, 294110, 294120, 294130, 294140, 294150, 294190, 294200, 300120, 300190, 300210, 300210, 300210, 300210, 300210, 300210, 300220, 300290, 300310, 300320, 300331, 300339, 300340, 300340, 300340, 300340, 300390, 300390, 300410, 300420, 300431, 300432, 300439, 300440, 300440, 300440, 300440, 300450, 300490, 300490, 300510, 300590, 300610, 300620, 300630, 300650, 300670, 300691, 300692, 340111, 340119, 340120, 340130, 340211, 340212, 340213, 340219, 340220, 340290, 350400, 350790, 370110, 370210, 380894, 382100, 382200, 382490, 390421, 391610, 391620, 391690, 392329, 392390, 392620, 392690, 401490, 401511, 401519, 401590, 481810, 481890, 560311, 560312, 560313, 560314, 560391, 560392, 560393, 560394, 560410, 560600, 590700, 600240, 600290, 611300, 611420, 611430, 611490, 611610, 621010, 621020, 621030, 621040, 621050, 621132, 621133, 621139, 621142, 621143, 621149, 621600, 621790, 630790, 650500, 650610, 701710, 701720, 701790, 721790, 732690, 760410, 760429, 761699, 841391, 841920, 842129, 842139, 842199, 847989, 854442, 900490, 901050, 901110, 901180, 901811, 901812, 901813, 901814, 901819, 901820, 901831, 901832, 901839, 901850, 901890, 901920, 902000, 902150, 902212, 902213, 902214, 902219, 902221, 902229, 902230, 902290, 902511, 902519, 902780, 903020, 940290, 961900, 390210)

# Retrieve interventions affecting these products in 2012
wb.hs= unique(c(hs.food, hs.med))
gta_data_slicer(gta.evaluation = c("Red","Amber"),
                hs.codes = wb.hs,
                implementation.period = c("2020-01-01", "2020-12-31"),
                intervention.type=c("Export ban"),
                keep.type = T,
                keep.implementation.na = F,
                keep.hs = T)



wb.interventions <- unique(master.sliced$intervention.id)

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

setnames(trade.coverage, "i.un","t.un")
coverage.exports=merge(subset(trade.coverage, affected.flow.id==2),
                       trade.base.bilateral,
                       by.x=c("t.un","hs6"),by.y = c("a.un","hs6"), all.x=T)
setnames(coverage.exports, "i.un","a.un")
setnames(coverage.exports, "t.un","i.un")
setnames(trade.coverage, "t.un","i.un")

trade.coverage.abs=coverage.exports

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

trade.per.hs.food=merge(aggregate(trade.value ~ hs4, unique(subset(trade.coverage.abs, hs6 %in% hs.food)),sum),
                        aggregate(trade.value ~ hs4, unique(subset(trade.base.bilateral, hs6 %in% hs.food)),sum),
                        by=c("hs4"), all=T)
trade.per.hs.food[is.na(trade.per.hs.food)]=0

trade.per.hs.food$trade.share=trade.per.hs.food$trade.value.x/trade.per.hs.food$trade.value.y
names(trade.per.hs.food)=c("HS code","Affected Trade Value","Global Exports","Affected Trade Share")
write.xlsx(trade.per.hs.food, file=paste0(output.path, "Trade coverage stats.xlsx"), sheetName = "Export shares - food", row.names = F)


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

