rm(list=ls())

library(gtalibrary)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)

gta_setwd()

project = '4 data queries'
query = '190604 Simon Panel US-Sino tariff war'
output.path = paste(project, query,'output/',sep='/')
data.path = paste(project, query,'data/',sep='/')

purple = "#9932CC"
master = xlsx::read.xlsx(file=paste0(data.path, 'Export volumes and prices.xlsx'), sheetIndex=1, startRow=3)
master = subset(master, select=!names(master) %in% c('NA.', 'NA..1'))
names(master) = gsub('X','',names(master))
names(master) = gsub('m0','-',names(master))
names(master) = gsub('m','-',names(master))
names(master) = paste0(names(master),'-1')
names(master) = as.Date(names(master),"%Y-%m-%d")
names(master)[1] = 'variable' 
master$variable = as.character(master$variable)
master[19,1] = 'World(after)'
master[20,1] = 'USA(after)'
  
master = subset(gather(master, year, value, names(master)[-1]), !is.na(variable))
master$year = as.Date(master$year)

gta_colour_palette()
vol.cols = c('USA (before)', 'USA (after)','World (before)','World (after)')
vol.data = subset(master, variable %in% vol.cols)
levels(vol.data$variable) = vol.cols

exp.vol.plot = ggplot(vol.data, aes(x=year, y=value, group=variable,colour=variable)) + 
  geom_line(size=1.05) +
  geom_smooth(data = subset(vol.data, variable == 'World (before)'), aes(x=year, y=value, group=variable,colour=variable), size=1.03 ,method='lm', se=F, linetype='twodash', fullrange = T) +
  geom_smooth(data = subset(vol.data, variable == 'USA (before)'), aes(x=year, y=value, group=variable,colour=variable), size = 1.03 ,method='lm', se=F, linetype='twodash', fullrange = T) +
  scale_color_manual(name = '',values=c(purple, gta_colour$red[2], gta_colour$green[2], gta_colour$blue[2])) + 
  scale_y_continuous(name = 'Import Volumes (April 2018=100)', breaks=seq(75.0,110.0,5.0), labels=paste0(seq(75.0,110.0,5.0),'.0') , limits=c(75,110), sec.axis = dup_axis()) +
  scale_x_date(name = "Month-Year", labels = date_format("%m-%Y"), date_breaks = "6 month", limits = c(min(master$year), max = max(master$year)), expand =c(0,0)) + 
  gta_theme() +
  guides(colour = guide_legend(override.aes = list(size=2.5)))
  
exp.vol.plot

price.cols = c('World', 'USA', 'World(after)', 'USA(after)')
price.data = subset(master, variable %in% price.cols)
price.labels = c('World (before)', 'USA (before)', 'World (after)', 'USA (after)')
price.data$variable = plyr::mapvalues(price.data$variable, as.character(price.cols), as.character(price.labels))
levels(price.data$variable) = price.labels

exp.price.plot = ggplot(price.data, aes(x=year, y=value, group=variable,colour=variable)) + 
  geom_line(size=1.05) +
  geom_smooth(data = subset(price.data, variable == 'World (before)'), aes(x=year, y=value, group=variable,colour=variable), size=1.03 ,method='lm', se=F, linetype='twodash', fullrange = T) +
  geom_smooth(data = subset(price.data, variable == 'USA (before)'), aes(x=year, y=value, group=variable,colour=variable), size = 1.03 ,method='lm', se=F, linetype='twodash', fullrange = T) +
  scale_color_manual(name = '',values=c(purple, gta_colour$red[2], gta_colour$green[2], gta_colour$blue[2])) + 
  scale_y_continuous(name = 'Import Prices (April 2018=100)', breaks=seq(85,115,5.0), labels=paste0(seq(85,115,5.0),'.0') , limits=c(85,115), sec.axis = dup_axis()) +
  scale_x_date(name = "Month-Year", labels = date_format("%m-%Y"), date_breaks = "6 month", limits = c(min(master$year), max = max(master$year)), expand =c(0,0)) + 
  gta_theme() +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

exp.price.plot

gta_plot_saver(plot=exp.vol.plot,
               path=output.path,
               name="Evolution of Import volumes (April 2018=100)")

gta_plot_saver(plot=exp.price.plot,
               path=output.path,
               name="Evolution of Import prices (April 2018=100)")

