library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")
library("ggplot2")
library("data.table")
library(scales)

rm(list=ls())

gta_setwd()

update.date='2019-09-04'

#changes made to the code: update.date line 14, and line 30-36 where I recalculate trade coverages based on the parameters
#given by the excel sheet of last time the figure was created

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")

cov.param <- openxlsx::read.xlsx(paste0(data.path, "2019-05-31 - GTA data dish #637.xlsx"), sheet = 2)

query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
data.path=paste0(query.path,'data/')

# GET TRADE COVERAGES
#get coverage parameters from previous data kitchen query

gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                   implementation.period=c('2008-11-01',update.date),
                   mast.chapters=c("P","TARIFF", "L"),
                   keep.mast = T,
                   group.mast = F)
                   
save(trade.coverage.estimates, file=paste0(data.path,'Fig 8 - Estimates.Rdata'))                   

load(paste0(data.path,'Fig 8 - Estimates.Rdata'))
coverages=trade.coverage.estimates
names(coverages)=gsub(' ','.',names(coverages))
# SHAPE FILE
coverages[,c("Importing.country" ,"Exporting.country" ,"Number.of.interventions.affecting.exported.product","MAST.chapter.ID")] <- NULL
names(coverages) <- c("MAST",2009:2019)
coverages <- gather(coverages, year, value, 2:ncol(coverages))
coverages$year <- as.numeric(coverages$year)

# PLOT
plot <- ggplot()+
  geom_line(data=coverages, aes(x=year, y=value, color=MAST),size=1)+
  scale_color_manual(values=gta_colour$qualitative[c(1:4)])+
  scale_x_continuous(breaks=c(2009:2019))+
  scale_y_continuous(breaks=seq(0,1,0.25), limits=c(-0.1,1), expand=c(0.05,0.05), sec.axis = dup_axis())+
  labs(x="Year",y="Share of world trade affected")+
  geom_text(data=subset(coverages, MAST %in% c("All included MAST chapters", "L: Subsidies (excl. export subsidies)")), aes(x=year, y=value, label=sprintf("%0.2f",round(value, 2))), nudge_y = 0.05, vjust=0, size=3)+
  geom_text(data=subset(coverages, MAST %in% c("P: Export-related measures (incl. subsidies)","Tariff measures")), aes(x=year, y=value, label=sprintf("%0.2f",round(value, 2))), nudge_y = -0.05, vjust=1, size=3)+
  guides(color = guide_legend(title = "MAST Chapters", ncol = 2, title.position = "top"))+
  gta_theme(x.bottom.angle = 45,x.bottom.align = 1)

plot

gta_plot_saver(plot=plot,
               path=output.path,
               name="Figure 8",
               eps=F)
