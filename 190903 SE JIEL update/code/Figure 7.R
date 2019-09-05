rm(list=ls())


library(gtalibrary)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(dplyr)
library(splitstackshape)

#changes to original script: only lines 22 (new cutoff date) & 37-41 adding new trade war intervention ids
#request: find how many jumbos there are per year and color those part of the trade war in a separate color

gta_setwd()

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
load("data/support tables/Final goods support table.Rdata")

jumbo.threshold = 10e9

cutoff="2019-09-04"
query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
data.path=paste0(query.path,'data/')

approach = 'conservative'

gta_data_slicer()
# "EC: GSP for certain countries' interventions as one
# 30038 state.act.id for EC: GSP for certain countries sectors revoked for 2014-2016 period
ec.revoked.gsp.ids = unique(subset(master.sliced, state.act.id == '30038')$intervention.id)
# indian export incentive 2.3 trillion 
false.jumbos = c(70350, 18891, 16819, 71578, 58794, 18254, 13633, 15366, 13512, 18892)
remove.ids = c(ec.revoked.gsp.ids,false.jumbos)

new.acts=c(37055,37190,37189,38163)
new.ids=c(71785,71833,72996,73056)
new.ids=c(new.ids,subset(master.sliced, state.act.id %in% new.acts)$intervention.id)

trade.war.intervention.ids=c(trade.war.intervention.ids,new.ids)


#JF: Please be sure that 73196 is removed from all trade war calculations
!73196 %in% trade.war.intervention.ids
#JF: alternatively, ensure that no intervention with an inception date in the future is counted
nrow(subset(master.sliced, intervention.id %in% trade.war.intervention.ids & date.implemented > Sys.Date()))


# find jumbos -------------------------------------------------------------
trade=subset(final, Year %in% c(2005:2007))[,c("Reporter.un","Partner.un","Year","Tariff.line","Value")]
rm(final)
names(trade)=c("i.un","a.un","year","affected.product","trade.value")

trade=aggregate(trade.value ~ i.un + a.un + affected.product, trade, sum)
trade$trade.value= trade$trade.value/3 

# gtalibrary::elig.firms
## preparing GTA data
gta_data_slicer(gta.evaluation = c("red","amber"),
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F)

# we need to implement sector-specific search in data slicer 
master.sliced = subset(master.sliced, eligible.firms %in% c('all','sector-specific'))

coverage.by.intervention=unique(master.sliced[,c("intervention.id","date.implemented","currently.in.force")])
coverage.by.intervention$year.implemented=year(coverage.by.intervention$date.implemented)
coverage.by.intervention$date.implemented=NULL
coverage.by.intervention$value.usd=NA
coverage.by.intervention$found.trade=T



master.temp=subset(master.sliced, intervention.id %in% coverage.by.intervention$intervention.id)

## Generating the base file 
gta_imp_exp_hs_tuples(master.path="master.temp",
                      master.data.frame = T)

master.tuple=merge(master.tuple, trade, by=c("i.un","a.un","affected.product"))

coverage.by.intervention$value.usd=NULL

coverage.by.intervention=merge(coverage.by.intervention, aggregate(trade.value ~ intervention.id, master.tuple, sum),by="intervention.id", all.x=T)
coverage.by.intervention$found.trade[is.na(coverage.by.intervention$trade.value)]=F
coverage.by.intervention$trade.value[is.na(coverage.by.intervention$trade.value)]=0

trade.coverage.base = subset(coverage.by.intervention, found.trade==T)

# remove ec.gsp.ids but keep highest 
trade.coverage.base = subset(trade.coverage.base, trade.value==max(subset(trade.coverage.base,intervention.id %in% ec.revoked.gsp.ids)$trade.value)| !(intervention.id %in% ec.revoked.gsp.ids))

# remove false jumbos
trade.coverage.base = subset(trade.coverage.base, !(intervention.id %in% false.jumbos))
## different subsets
ids.all=unique(master.sliced$intervention.id)
ids.conservative=unique(subset(master.sliced, implementation.level %in% c("national", "supranational") &
                                 eligible.firms %in% c("all", "sector-specific"))$intervention.id)


trade.jumbo.intervention = subset(trade.coverage.base, trade.value >= jumbo.threshold & intervention.id %in% ids.conservative)
jumbo.ids = unique(trade.jumbo.intervention$intervention.id)
save(jumbo.ids, trade.jumbo.intervention, file = paste0(data.path,"/trade per jumbo intervention.Rdata"))

load(paste0(data.path,"/trade per jumbo intervention.Rdata"))
# plot --------------------------------------------------------------------

gta_colour_palette()
loop.data=subset(trade.coverage.base, intervention.id %in% ids.conservative)

threshold = jumbo.threshold

annual.jumbos=aggregate(intervention.id ~ year.implemented, subset(loop.data, trade.value>=threshold & !intervention.id %in% trade.war.intervention.ids), function(x) length(unique(x)))
annual.jumbos$intervention.status="2"
tw.jumbos = subset(loop.data, trade.value>=threshold & intervention.id %in% trade.war.intervention.ids)
if (nrow(tw.jumbos)>0){
  tw.jumbos=aggregate(intervention.id ~ year.implemented, tw.jumbos , function(x) length(unique(x)))
  tw.jumbos$intervention.status="1"
  annual.jumbos=rbind(tw.jumbos,annual.jumbos)
  color.values = c(gta_colour$qualitative[2:1])
} else {color.values=gta_colour$qualitative[1]}

fig.7 =ggplot(annual.jumbos, aes(x=year.implemented,y=intervention.id,fill=intervention.status)) + geom_col() + 
  scale_x_continuous(breaks=2008:2019,labels=2008:2019) + xlab('Year of implementation of the harmful intervention') +
  ylab(paste('Number of jumbo protectionist measures implemented')) +
  scale_fill_manual(name='',values = color.values, labels=c('Trade war interventions','Non-trade war interventions')) +
  gta_theme() +
  scale_y_continuous(breaks=c(0,10,20,30,40,50),labels=c(0,10,20,30,40,50),sec.axis = dup_axis()) +
  coord_cartesian(ylim = c(0,52))


fig.7

gta_plot_saver(plot=fig.7,
               path=output.path,
               name='Figure 7')


