rm(list=ls())

library(gtalibrary)
library(ggplot2)
library(tidyverse)

gta_colour_palette()
project = '4 data queries'
query = '190604 Simon Panel US-Sino tariff war'
output.path = paste(project, query,'output/',sep='/')
data.path = paste(project, query,'data/',sep='/')

load(paste0(data.path, 'Sino-US panel data - JF.Rdata'))
load("4 data queries/190604 Simon Panel US-Sino tariff war/data/P estimates.Rdata")

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

master=subset(master, year!="2019")
master=subset(master, mast!="P")
master$importer[master$importer=="China"]="Chinese impediments to US exports"
master$importer[master$importer=="United States of America"]="US impediments to Chinese exports"


master$mast[master$mast=="ALL"]="All impediments"
master$mast[master$mast=="L"]="Subsidies to\nimport-competing firms"
master$mast[master$mast=="TARIFF"]="Import tariffs"

## creating a joint variable is necessary so I can plot it all in a single ggplot command.
master$plotted.variable=master$target.status
master$plotted.variable[master$which.panel==2]=master$mast[master$which.panel==2]

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
                       sec.axis = dup_axis())+
    scale_color_manual(values=my.colours, 
                       breaks=c("any","targeted","untargeted"))+ ## 'breaks' specifies which values are displayed in the legend
    scale_size_manual(values=rep(1.2, length(my.colours)), 
                      breaks=c("All impediments","Subsidies to\nimport-competing firms","Import tariffs"))+
    labs(y="Share of imports affected",
         x="", 
         colour="targeted or not", 
         size="instrument used")+
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
               name="Sino-US panel - full panel")



