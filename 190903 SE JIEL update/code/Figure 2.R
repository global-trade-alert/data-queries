rm(list=ls())

library(gtalibrary)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(scales)

gta_setwd()

query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
data.path=paste0(query.path,'data/')
  
cutoff.date=as.Date('2018-04-01')
#Important remark: slightly different numbers (tenth of a decimal point different here and there)
#even for old years, for example 2013-02 86.7 vs 86.5. How can the old data points have changed?
#i ensured that the variables are the correct ones, 'mgz_wl_qnmi_sn' etc.
#previous excel file which is under the prev figs&data folder was provided by simon with already some work done
#but he did not openly change numbers

#Perceived task: recreate the given graphs
#for world imports and the united states, for both prices and volumes
#index 2018=100 and create evolution line of volume/price in separate graphs
#differentiate by color pre-2018 april and post 2018 april
#add linear regression for volumes, and polynomial(deg2) one for prices


purple = "#9932CC"
master = openxlsx::read.xlsx(xlsxFile=paste0(data.path, 'wtm_2019m06.xlsx'), sheet=1, startRow=3)
master = subset(master, select=!names(master) %in% c('X2', 'X3'))
names(master) = paste0(gsub('m','-',names(master)),'-1')
names(master) = as.Date(names(master),"%Y-%m-%d")
names(master)[1] = 'variable' 
master$variable = as.character(master$variable)
master$variable=str_trim(master$variable)
master[3,1]='Volumes, World'
master[5,1]='Volumes, USA'
master[29,1]='Prices, World'
master[31,1]='Prices, USA'

master=subset(master, variable %in% c('Volumes, World','Volumes, USA','Prices, World','Prices, USA'))

#2018=100 like the original graph was
master[1,-1]=master[1,-1]/master$`2018-04-01`[1]*100
master[2,-1]=master[2,-1]/master$`2018-04-01`[2]*100
master[3,-1]=master[3,-1]/master$`2018-04-01`[3]*100
master[4,-1]=master[4,-1]/master$`2018-04-01`[4]*100

master = subset(gather(master, year, value, names(master)[-1]), !is.na(variable))
master$year = as.Date(master$year)

master=subset(master,year>=as.Date('2013-01-01'))

master$variable[master$year<=cutoff.date]=paste(master$variable[master$year<=cutoff.date],'(before)')
master$variable[master$year>cutoff.date]=paste(master$variable[master$year>cutoff.date],'(after)')
master$value=as.numeric(master$value)

volumes=master[grep('Volumes',master$variable),]
prices=master[grep('Prices',master$variable),]

volumes$variable=gsub('Volumes, ','',volumes$variable)
prices$variable=gsub('Prices, ','',prices$variable)


gta_colour_palette()

levels(volumes$variable) = c('USA (before)', 'USA (after)','World (before)','World (after)')
levels(prices$variable) = c('USA (before)', 'USA (after)','World (before)','World (after)')




vol.plot = ggplot(volumes, aes(x=year, y=value, group=variable,colour=variable)) + 
  geom_line(size=1.05) +
  geom_smooth(data = subset(volumes, variable == 'World (before)'), aes(x=year, y=value, group=variable,colour=variable), size=1.03 ,method='lm', se=F, linetype='twodash', fullrange = T) +
  geom_smooth(data = subset(volumes, variable == 'USA (before)'), aes(x=year, y=value, group=variable,colour=variable), size = 1.03 ,method='lm', se=F, linetype='twodash', fullrange = T) +
  scale_color_manual(name = '',values=c(purple, gta_colour$red[2], gta_colour$green[2], gta_colour$blue[2])) +
  scale_y_continuous(name = 'Import Volumes (April 2018=100)', breaks=seq(75.0,110.0,5.0), labels=paste0(seq(75.0,110.0,5.0),'.0') , limits=c(75,110), sec.axis = dup_axis()) +
  scale_x_date(name = "Month-Year", labels = date_format("%m-%Y"), date_breaks = "6 month", limits = c(min(master$year), max = max(master$year)), expand =c(0,0)) +
  gta_theme() +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

vol.plot

price.plot = ggplot(prices, aes(x=year, y=value, group=variable,colour=variable)) + 
  geom_line(size=1.05) +
  stat_smooth(data = subset(prices, variable == 'World (before)'), aes(x=year, y=value, group=variable,colour=variable), size=1.03 ,method='lm', se=F, formula =y ~ poly(x,2,raw=T),linetype='twodash', fullrange = T) +
  geom_smooth(data = subset(prices, variable == 'USA (before)'), aes(x=year, y=value, group=variable,colour=variable), size = 1.03 ,method='lm', se=F, formula =y ~ poly(x,2,raw=T), linetype='twodash', fullrange = T) +
  scale_color_manual(name = '',values=c(purple, gta_colour$red[2], gta_colour$green[2], gta_colour$blue[2])) + 
  scale_y_continuous(name = 'Import Prices (April 2018=100)', breaks=seq(85,115,5.0), labels=paste0(seq(85,115,5.0),'.0') , limits=c(85,115), sec.axis = dup_axis()) +
  scale_x_date(name = "Month-Year", labels = date_format("%m-%Y"), date_breaks = "6 month", limits = c(min(master$year), max = max(master$year)), expand =c(0,0)) + 
  gta_theme() +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

price.plot

gta_plot_saver(plot=vol.plot,
               path=output.path,
               name="fig 2.1 - Evolution until 1-5-19 of Import volumes (April 2018=100)",
               eps=F)

gta_plot_saver(plot=price.plot,
               path=output.path,
               name="fig 2.2 - Evolution until 1-5-19 of Import prices (April 2018=100)",
               eps=F)

