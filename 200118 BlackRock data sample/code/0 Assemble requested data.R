rm(list=ls())

library(gtalibrary)
library(gtasql)
library(pool)
library(plyr)

gta_setwd()

source('setup/keys/gtamain.R')
gta_sql_pool_open(table.prefix = 'gta_', db.title = db.name, db.host = db.host, db.name = db.name, db.user = db.user, db.password = db.password)
query.path = '4 data queries/200118 BlackRock data sample/'
data.path = paste0(query.path, 'data/')
output.path = paste0(query.path, 'output/')

# Req: We are interested in the time-series of both (harmful and liberalizing).
#      Import barriers and export subsidies should be fine.
#      Restrict to countries provided
#      Trade data includes values of year prior to announcement, moving average is done with year prior to announcement and two years prior
#      Trade data include int.id/cty.impl/cty.imp/cty.exp/date.ann/date.impl/date.rem/prod.code/trade.val/sect.code/value.new/value.unit/value.tl.specific [where available]
#      Meta data include int.id/int.type/gta.eval/mast.chapt/impl.level/firm.specificity


jur.list = gta_sql_load_table('jurisdiction')
orig.cty.list = readxl::read_xlsx(paste0(data.path,'dm_em_country_list.xlsx'),sheet=1)$Countries
gta.cty.list = mapvalues(orig.cty.list, c("UTD ARAB EM","CZECH REPUBLIC","KOREA","NETHERLAND","TAIWAN","UNITED STATES"),
                                    c("United Arab Emirates","Czechia","Republic of Korea","Netherlands","Chinese Taipei","United States of America"))
cty.list.un = as.numeric(mapvalues(toupper(gta.cty.list), toupper(jur.list$name), jur.list$un.code))
cty.list.id = mapvalues(cty.list.un, jur.list$un.code, jur.list$id)
# cty.list.un[!cty.list.un %in% jur.list$un]

# Meta data assembly ---------------------------------------------------------------

# later restrict to those interventions which we provide in the trade value data? 

pull.meta.q= 'SELECT gta_intervention.id intervention_id, gta_measure_type.name intervention_type, gta_evaluation.label gta_evaluation, 
          	  gta_mast.frontpage_chapter mast_chapter, gta_implementation_level.label implementation_level, gta_eligible_firms.label firms_specificity
              FROM gta_intervention, gta_measure_type, gta_mast, gta_evaluation, gta_implementation_level, gta_eligible_firms
              WHERE gta_intervention.measure_type_id = gta_measure_type.id
              AND gta_measure_type.mast_id = gta_mast.id
              AND gta_intervention.evaluation_id = gta_evaluation.id
              AND gta_intervention.implementation_level_id = gta_implementation_level.id
              AND gta_intervention.eligible_firms_id = gta_eligible_firms.id;'
main.meta.data = gta_sql_get_value(pull.meta.q)
main.meta.data = main.meta.data[order(main.meta.data$intervention.id),]

gta_data_slicer()
# 
# slicer.meta.data = unique(subset(master.sliced, select=c('intervention.id','intervention.type','gta.evaluation','mast.chapter','implementation.level','eligible.firms')))
# slicer.meta.data = slicer.meta.data[order(slicer.meta.data$intervention.id),]
# 
# length(setdiff(main.meta.data$intervention.id, slicer.meta.data$intervention.id))
# length(setdiff(slicer.meta.data$intervention.id, main.meta.data$intervention.id))


# Assemble trade value data  --------------------------------------------------------

# gta_distorted_market what does this table indicate? int.id / jur.id / type (Deleted / Normal / Added)

cty.ids = toString(sprintf("'%s'", cty.list.id))

pull.it.rev = paste("SELECT gta_it_revised.intervention_id, i_un.un_code i_un, d_un.un_code d_un,",
                    "a_un.un_code a_un, gta_it_revised.sector_code_3, gta_measure_type.name, gta_tariff_line.code hs6",
                    "FROM gta_it_revised, gta_intervention, gta_measure_type, gta_tariff_line, gta_jurisdiction i_un, gta_jurisdiction d_un, gta_jurisdiction a_un",
                    "WHERE gta_it_revised.intervention_id = gta_intervention.id",
                    "AND gta_intervention.measure_id IN (SELECT id FROM gta_measure WHERE status_id = 4)",
                    "AND gta_intervention.measure_type_id = gta_measure_type.id",
                    "AND gta_intervention.affected_flow_id IN (1,3)",
                    "AND gta_it_revised.tariff_line_id = gta_tariff_line.id",
                    "AND gta_it_revised.implementing_jurisdiction_id = i_un.id",
                    "AND gta_it_revised.distorted_market_id = d_un.id",
                    "AND gta_it_revised.affected_jurisdiction_id = a_un.id",
                    "AND (gta_it_revised.implementing_jurisdiction_id IN (%s) OR gta_it_revised.distorted_market_id IN (%s) OR gta_it_revised.affected_jurisdiction_id IN (%s));")

pull.it.rev = do.call(sprintf, c(list(pull.it.rev), c(cty.ids,cty.ids,cty.ids)))
it.rev = gta_sql_multiple_queries(pull.it.rev, output.queries = 1) # down from 19m to 9m rows

trade.value.data = it.rev
trade.value.data$country.implementing = trade.value.data$i.un
trade.value.data$country.importing = trade.value.data$d.un #d.un is un.code of distorted.market
trade.value.data$country.exporting = trade.value.data$a.un

# merge date.ann 
trade.value.data = merge(trade.value.data, unique(subset(master.sliced, select=c('intervention.id','date.announced'))), all.x = T,
                         by='intervention.id')

trade.value.data = subset(trade.value.data, !(is.na(date.announced)))

## adding treatment values and accounting for tariff line specific implementation/removal dates
tl.treatment=unique(trade.value.data[,c("intervention.id","hs6")])
gta.atl=gta_sql_get_value(paste0("SELECT intervention_id, tariff_line_code hs6, prior_level, new_level, unit, inception_date date_implemented, removal_date date_removed
                                  FROM gta_affected_tariff_line
                                  WHERE intervention_id IN (",paste(unique(tl.treatment$intervention.id), collapse=","),")"))
gta.atl$prior.level[gta.atl$prior.level==""]=NA
gta.atl$new.level[gta.atl$new.level==""]=NA


tl.treatment=merge(tl.treatment, gta.atl, by=c("intervention.id","hs6"), all.x=T)

### adding aggregate implementation/removal dates
tl.no.inception=subset(tl.treatment, is.na(date.implemented))
tl.no.inception$date.implemented=NULL
tl.no.inception=merge(tl.no.inception, unique(master.sliced[,c("intervention.id","date.implemented")]), by="intervention.id", all.x=T)
tl.treatment=rbind(subset(tl.treatment, is.na(date.implemented)==F),
                   tl.no.inception)
rm(tl.no.inception)

tl.no.removal=subset(tl.treatment, is.na(date.removed))
tl.no.removal$date.removed=NULL
tl.no.removal=merge(tl.no.removal, unique(master.sliced[,c("intervention.id","date.removed")]), by="intervention.id", all.x=T)
tl.treatment=rbind(subset(tl.treatment, is.na(date.removed)==F),
                   tl.no.removal)
rm(tl.no.removal)



### adding aggregate prior/new level values & unit where necessary
tl.with.treatement=subset(tl.treatment, !(is.na(prior.level) & is.na(new.level)))
tl.with.treatement$product.specific.treatment=T

tl.no.treatement=subset(tl.treatment, is.na(prior.level) & is.na(new.level))
tl.no.treatement$new.level=NULL
tl.no.treatement$prior.level=NULL
tl.no.treatement$unit=NULL
tl.no.treatement$product.specific.treatment=F
gta.int=gta_sql_get_value(paste0("SELECT id intervention_id, prior_level, new_level, unit
                                  FROM gta_intervention
                                  WHERE id IN (",paste(unique(tl.no.treatement$intervention.id), collapse=","),")
                                  AND unit >0"))
gta.int$new.level=as.numeric(as.character(gta.int$new.level))
gta.int$prior.level=as.numeric(as.character(gta.int$prior.level))
gta.int=subset(gta.int, is.na(new.level)==F | is.na(prior.level)==F)

tl.no.treatement=merge(tl.no.treatement, gta.int, by="intervention.id", all.x=T)
tl.treatment=rbind(tl.with.treatement,
                   tl.no.treatement)

rm(tl.with.treatement,tl.no.treatement)

## unit.cleaning
tl.treatment$unit[grepl("%", tl.treatment$prior.level)|grepl("%", tl.treatment$new.level)]=1
tl.treatment$prior.level=gsub("%","",tl.treatment$prior.level)
tl.treatment$new.level=gsub("%","",tl.treatment$new.level)
tl.treatment$new.level=as.numeric(as.character(tl.treatment$new.level))
tl.treatment$prior.level=as.numeric(as.character(tl.treatment$prior.level))
tl.treatment$unit[is.na(tl.treatment$new.level)|is.na(tl.treatment$prior.level)]=NA
tl.treatment$unit[tl.treatment$unit==0]=1

unit.df=data.frame(unit=c(1:19),
                   unit.type=c("percent", "budget (USD)", "budget (USD)", "USD/MT", "USD/kg", "USD/tonne", "USD/unit", "count", "USD/litre", "USD/pc", "USD/squaremetre", "USD/pair", "USD/ldt", "USD/lb", "USD/gallon", "USD/hl", "USD/LAL", "USD/tyre", "USD/stick"),
                   stringsAsFactors = F)
tl.treatment=merge(tl.treatment, unit.df, by="unit", all.x=T)

## dates cleanring (somthing went wrong above)
tl.treatment$date.implemented[!grepl("-",tl.treatment$date.implemented)]=as.character(as.Date(as.numeric(tl.treatment$date.implemented[!grepl("-",tl.treatment$date.implemented)]), origin="1970-01-01"))
tl.treatment$date.removed[!grepl("-",tl.treatment$date.removed)]=as.character(as.Date(as.numeric(tl.treatment$date.removed[!grepl("-",tl.treatment$date.removed)]), origin="1970-01-01"))
tl.treatment$date.implemented=as.Date(tl.treatment$date.implemented, "%Y-%m-%d")
tl.treatment$date.removed=as.Date(tl.treatment$date.removed, "%Y-%m-%d")
tl.treatment$unit=NULL


trade.value.data=merge(trade.value.data, tl.treatment, by=c("intervention.id", "hs6"), all.x=T)
### Adding trade values

# preparing to attach trade values
trade.value.data$t.data = year(trade.value.data$date.announced)-1

# accounting for interventions announced in 2020 that do not have trade data in the prior year.
interventions.2019=unique(trade.value.data$intervention.id[trade.value.data$t.data==2019])
trade.value.data$t.data[trade.value.data$t.data==2019]=2018

req.years = unique(trade.value.data$t.data)
t.base = data.frame()
for(yr in 2005:2018){
  gta_trade_value_bilateral(trade.data = yr)
  trade.base.bilateral$yr = yr
  t.base = rbind(t.base, 
                 trade.base.bilateral)
  
  rm(trade.base.bilateral)
}

# moving avg of previous 3 years 
mov.avg = lapply(req.years, function(x) aggregate(trade.value~i.un+a.un+hs6, subset(t.base, yr %in% (x-2):x), 
                                                  mean))
for(i in seq_along(mov.avg)) mov.avg[[i]]$yr = req.years[i]
mov.avg = do.call("rbind", mov.avg)
setnames(mov.avg,'trade.value','trade.value.mov.avg')

trade.value.data = merge(trade.value.data, t.base, 
                         by.x=c('i.un','a.un','hs6','t.data'),
                         by.y=c('i.un','a.un','hs6','yr'),
                         all.x = T)

trade.value.data = merge(trade.value.data, mov.avg, 
                         by.x=c('i.un','a.un','hs6','t.data'),
                         by.y=c('i.un','a.un','hs6','yr'),
                         all.x = T)

trade.value.data$trade.value[is.na(trade.value.data$trade.value)] = 0
trade.value.data$trade.value.mov.avg[is.na(trade.value.data$trade.value.mov.avg)] = 0

# map names back to the ones provided
trade.value.data$country.implementing = mapvalues(trade.value.data$country.implementing, jur.list$un.code, jur.list$name)
trade.value.data$country.importing = mapvalues(trade.value.data$country.importing, jur.list$un.code, jur.list$name)
trade.value.data$country.exporting = mapvalues(trade.value.data$country.exporting, jur.list$un.code, jur.list$name)

trade.value.data$country.implementing = mapvalues(toupper(trade.value.data$country.implementing), toupper(gta.cty.list), orig.cty.list)
trade.value.data$country.importing = mapvalues(toupper(trade.value.data$country.importing), toupper(gta.cty.list), orig.cty.list)
trade.value.data$country.exporting = mapvalues(toupper(trade.value.data$country.exporting), toupper(gta.cty.list), orig.cty.list)

trade.value.data = subset(trade.value.data, select=c('intervention.id','country.implementing','country.importing','country.exporting','date.announced',
                                                     'date.implemented','date.removed','hs6',"prior.level","new.level","unit.type",'trade.value','trade.value.mov.avg','sector.code.3'))
data.table::setnames(trade.value.data, c('hs6','sector.code.3'),c('product.code','sector.code'))





# Save files --------------------------------------------------------------
main.meta.data = subset(main.meta.data, intervention.id %in% trade.value.data$intervention.id)

write.csv(main.meta.data, file = paste0(output.path,'MetaData.csv'))
write.csv(trade.value.data, file = paste0(output.path,'TradeData.csv'))
