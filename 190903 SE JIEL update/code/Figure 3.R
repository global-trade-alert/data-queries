rm(list=ls())

library(gtalibrary)

#changes made to original code: 
#implementation.period = c(.., date.of.update)
#removal of year 2019 in line 128 outcommented 
#the chinese impediments to US exports is largely different from when produced
#due to the intervention https://www.globaltradealert.org/intervention/71826
#i have removed it
#set remove.id to NULL to include it, or 71826 to remove it
remove.id=NULL

gta_setwd()

query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
data.path=paste0(query.path,'data/')

date.of.update='2019-09-05'

coverage.period = c(2009,2019)
gta.evaluation = c('Red','Amber')
intra.year.duration = T


mast.chapters=c("All included MAST chapters","TARIFF", "L")

## ANY for targeted, instrument chapters ALL, L and TARIFF
gta_trade_coverage(gta.evaluation = gta.evaluation,
                   coverage.period = coverage.period,
                   implementation.period = c("2008-11-01",date.of.update),
                   affected.flow="inward",
                   exporters=c(156,840),
                   keep.exporters = T,
                   group.exporters = F,
                   importers = c(156,840),
                   keep.importers = T,
                   group.importers = F,
                   group.mast = F,
                   intra.year.duration = T,
                   intervention.ids = remove.id,
                   keep.interventions = F)

any.target.any.inst=subset(trade.coverage.estimates, `MAST chapter ID` %in% mast.chapters)
any.target.any.inst$target.status="any"

## targeted, instrument chapters ALL
gta_trade_coverage(gta.evaluation = gta.evaluation,
                   coverage.period = coverage.period,
                   implementation.period = c("2008-11-01", date.of.update),
                   affected.flow="inward",
                   exporters=c(156,840),
                   keep.exporters = T,
                   group.exporters = F,
                   importers = c(156,840),
                   keep.importers = T,
                   group.importers = F,
                   group.mast = T,
                   nr.exporters = c(1,1),
                   intra.year.duration = T,
                   intervention.ids = remove.id,
                   keep.interventions = F)

targeted=trade.coverage.estimates
targeted$`MAST chapter name`="All included MAST chapters"
targeted$`MAST chapter ID`="All included MAST chapters"
targeted$target.status="targeted"

## untargeted, instrument chapters ALL
gta_trade_coverage(gta.evaluation = gta.evaluation,
                   coverage.period = coverage.period,
                   implementation.period = c("2008-11-01", date.of.update),
                   affected.flow="inward",
                   exporters=c(156,840),
                   keep.exporters = T,
                   group.exporters = F,
                   importers = c(156,840),
                   keep.importers = T,
                   group.importers = F,
                   group.mast = T,
                   nr.exporters = c(2,9999),
                   intra.year.duration = T,
                   intervention.ids = remove.id,
                   keep.interventions = F)

untargeted=trade.coverage.estimates

untargeted$`MAST chapter name`="All included MAST chapters"
untargeted$`MAST chapter ID`="All included MAST chapters"
untargeted$target.status="untargeted"

master=rbind(any.target.any.inst, targeted, untargeted)
names(master)=gsub("Trade coverage estimate for ","", names(master))
master$`Number of interventions affecting exported product`=NULL
xlsx::write.xlsx(master, file=paste(output.path, "fig 3 - Data for panel plot.xlsx"), row.names = F, sheetName = "all except P")

## getting in P data
load(paste0(data.path,"P estimates.Rdata"))
trade.coverage.estimates=reshape(trade.coverage.estimates, idvar=c("importer","exporter","mast","target.status"), timevar = "year", direction="wide")
names(trade.coverage.estimates)=gsub("value\\.","", names(trade.coverage.estimates))
xlsx::write.xlsx(trade.coverage.estimates, file=paste(output.path, "fig 3 - Data for panel plot.xlsx"), row.names = F, sheetName = "MAST P", append=T)


save(master, file=paste0(data.path, 'Sino-US panel data - JF.Rdata'))


# plotting ----------------------------------------------------------------

rm(list=ls())

library(gtalibrary)
library(ggplot2)
library(tidyverse)
gta_setwd()

gta_colour_palette()
query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
data.path=paste0(query.path,'data/')

load(paste0(data.path, 'Sino-US panel data - JF.Rdata'))
load(paste0(data.path,"P estimates.Rdata"))
     
master = gather(unique(master), year, value, as.character(2009:2019))
names(master)=c("importer","exporter", "mast","mast.name","target.status","year","value")
master$mast.name=NULL
master$mast[master$mast=="All included MAST chapters"]="ALL"
master = rbind(master, trade.coverage.estimates)

master$target.status=tolower(master$target.status)
master$which.panel=2
master$which.panel[master$target.status!="any"]=1

all.any=subset(master, target.status=="any" & mast=="ALL")
all.any$which.panel=1

master=rbind(master, all.any)

# master=subset(master, year!="2019")
master=subset(master, mast!="P")
master$importer[master$importer=="China"]="Chinese impediments to US exports"
master$importer[master$importer=="United States of America"]="US impediments to Chinese exports"


master$mast[master$mast=="ALL"]="All impediments"
master$mast[master$mast=="L"]="Subsidies to\nimport-competing firms"
master$mast[master$mast=="TARIFF"]="Import tariffs"

## creating a joint variable is necessary so I can plot it all in a single ggplot command.
master$plotted.variable=master$target.status
master$plotted.variable[master$which.panel==2]=master$mast[master$which.panel==2]
master$plotted.variable=tolower(master$plotted.variable)

my.colours=c(gta_colour$qualitative[c(1,1,2,4,3,7)])

full.panel=
 ggplot(master, 
        aes(x=year, 
            y=value, 
            group=plotted.variable, 
            colour=plotted.variable, 
            size=plotted.variable)
 )+
 geom_line()+
 facet_grid(which.panel ~ importer)+
 gta_theme(x.bottom.angle =90)+
 theme(axis.text.x = element_text(vjust=.5),
       strip.background.x = element_rect(fill = gta_colour$panel.bg),
       strip.background.y = element_blank(),  
       strip.text.y = element_blank())+
 scale_y_continuous(limits=c(0,1),
                    breaks=seq(0,1,.2),
                    sec.axis = dup_axis())+
 scale_color_manual(values=my.colours, 
                    breaks=c("any","targeted","untargeted"))+ ## 'breaks' specifies which values are displayed in the legend
 scale_size_manual(values=rep(1.2, length(my.colours)), 
                   breaks=c("all impediments","subsidies to\nimport-competing firms","import tariffs"))+
 labs(y="Share of exports affected",
      x="", 
      colour="targeted or not\n(top row)", 
      size="instrument used\n(bottom row)")+
 guides(size=guide_legend(order=0, # I can set the order of the legend from left to right
                          title.position = "top",
                          nrow=3,
                          override.aes = list(size=1.2,
                                              colour=c(my.colours[c(1,4,3)]))), ## allows me to change size of the symbol and its colours etc as I like
        color=guide_legend(order=1, 
                           title.position = "top",
                           nrow=3,
                           override.aes = list(size=1.2))
 )

full.panel

gta_plot_saver(plot=full.panel,
              path=output.path,
              aspect.ratio = 1.3,
              name="fig 3 - Sino-US panel - full panel",
              eps=F)
     
     
     
     
