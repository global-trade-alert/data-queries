rm(list=ls())

library(gtalibrary)
library(ggplot2)

project = '4 data queries'
query = '190604 Simon Panel US-Sino tariff war'
output.path = paste(project, query,'output/',sep='/')
data.path = paste(project, query,'data/',sep='/')

load(paste0(data.path, 'Sino-US panel data.Rdata'))
load("4 data queries/190604 Simon Panel US-Sino tariff war/data/P estimates.Rdata")

master = gather(unique(master), year, value, as.character(2009:2019))
master = rbind(master, trade.coverage.estimates)
xlsx::write.xlsx(master, file = paste0(output.path, 'Sino-US panel data.xlsx'), sheetName = 'Sheet1')

#change facet name
levels(master$importer) = c('US interventions affecting China', 'Chinese Interventions affecting the US')

gta_colour_palette()

tar.plots = ggplot(data = subset(master, mast == 'any'), aes(x=year, y=value, group=target.status, colour=target.status)) + facet_wrap(~importer) +
  geom_line(size = 1.1)
  
tar.plots

p.mast.plots = ggplot(data = subset(master, target.status == 'Any'), aes(x=year, y=value, group=mast, colour=mast)) + facet_wrap(~importer) +
  geom_line(size = 1.1) 
  
p.mast.plots

without.p.plots = ggplot(data = subset(master, target.status == 'Any' & mast!='P'), aes(x=year, y=value, group=mast, colour=mast)) + facet_wrap(~importer) +
  geom_line(size = 1.1) 

without.p.plots

gta_plot_saver(plot=tar.plots,
               path=output.path,
               name="Sino-US panel targets")

gta_plot_saver(plot=p.mast.plots,
               path=output.path,
               name="Sino-US mast chapters with P")

gta_plot_saver(plot=without.p.plots,
               path=output.path,
               name="Sino-US mast chapters without P")

# inspection into kink in P from china  -----------------------------------------------------------------------

gta_data_slicer()
master.sliced = subset(master.sliced, a.un == 156 & i.un == 840 & year(date.implemented) %in% c(2018,2019) & mast.chapter == 'P')
# 70295 intervention n° most likely
