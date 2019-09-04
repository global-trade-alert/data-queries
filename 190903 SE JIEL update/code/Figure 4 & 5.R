rm(list=ls())


library(xlsx)
library(gtalibrary)
library(ggplot2)
library(tidyverse)

#changes: line 37, for yr in ... 2019 instead of 2018
#         line 188, for yr in ... 2019 instead of 2018
#         line 166 year breaks ... 2019 instead of 2018
#         line 297 year breaks ... 2019 instead of 2018
#  trade.data=ifelse(yr==2019,yr-2,yr-1) line 48, uses 2017 trade data for yr=2019
# same for line 200, but also 'prior year' changed to yr-1 or yr-2 if year 2019

# ks: outcommented, extrafonts doesnt exist in 3.6.1
# font_import()
# loadfonts(device="postscript")
# loadfonts(device="win")

gta_setwd()

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")
source("0 report production/GTA 23/help files/GTA 23 cutoff and definitions.R")

query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
data.path=paste0(query.path,'data/')

# data collection fig 4 ---------------------------------------------------

# F1: For each year 2009-2019, 
# (i) total value of trade affected by US and Chinese tariff hikes (including trade defence actions) on each other only imposed in a given year, 
# (ii) total value of trade affected by world's tariff hikes (including trade defence actions) harming only one other party imposed in a given year, 
# (iii) total value of trade affected by world's tariff hikes (including trade defence actions) harming any number of trading partners and 
# (iv) total value of world imports harmed by any import distortion affecting any number of trading partners.

figure.4=data.frame(year=numeric(),
                     i=numeric(),
                     ii=numeric(),
                     iii=numeric(),
                     iv=numeric())

tariffs.et.al=as.character(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"])

for(yr in c(2009:2019)){
  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")
  trade.data=ifelse(yr==2019,yr-2,yr-1)
  
  f31=data.frame(year=yr,
                 i=NA,
                 ii=NA,
                 iii=NA,
                 iv=NA)
  
  ## i
  # (i) total value of trade affected by US and Chinese tariff hikes (including trade defence actions) on each other only imposed in a given year, 
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     importers = c("China","United States of America"),
                     keep.importers = T,
                     exporters = c("China","United States of America"),
                     keep.exporters = T,
                     nr.exporters = c(1,1),
                     intervention.types = tariffs.et.al,
                     keep.type = T,
                     group.type = T,
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f31$i=trade.coverage.estimates[,3]
  f31$i=trade.coverage.estimates[,4]
  
  # (ii) total value of trade affected by world's tariff hikes (including trade defence actions) harming only one other party imposed in a given year, 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     nr.exporters = c(1,1),
                     intervention.types = tariffs.et.al,
                     keep.type = T,
                     group.type = T,
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f31$ii=trade.coverage.estimates[,3]
  f31$ii=trade.coverage.estimates[,4]
  
  
  # (iii) total value of trade affected by world's tariff hikes (including trade defence actions) harming any number of trading partners and 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     intervention.types = tariffs.et.al,
                     keep.type = T,
                     group.type = T,
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f31$iii=trade.coverage.estimates[,3]
  f31$iii=trade.coverage.estimates[,4]
  
  
  # (iv) total value of world imports harmed by any import distortion affecting any number of trading partners.
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f31$iv=trade.coverage.estimates[,3]
  f31$iv=trade.coverage.estimates[,4]
  
  
  
  figure.4=rbind(figure.4, f31)
  print(yr)
}

# SAVE DATASET
save(figure.4, file=paste0(data.path,"coveragesFigure4.Rdata"))


# plot fig 4 --------------------------------------------------------------

load(paste0(data.path,"coveragesFigure4.Rdata"))


figure.4.plot <- figure.4

figure.4.plot$bilateral.tariff.hikes <- round(100*(figure.4.plot$i/figure.4.plot[nrow(figure.4.plot),c("i")]))
figure.4.plot$all.tariff.hikes.single.nation <- round(100*(figure.4.plot$ii/figure.4.plot[nrow(figure.4.plot),c("i")]))
figure.4.plot$all.tariff.hikes <- round(100*(figure.4.plot$iii/figure.4.plot[nrow(figure.4.plot),c("i")]))
figure.4.plot$all.import.restrictions <- round(100*(figure.4.plot$iv/figure.4.plot[nrow(figure.4.plot),c("i")]))

figure.4.xlsx=figure.4

figure.4.xlsx <- cbind(figure.4.xlsx, figure.4.plot[,c("bilateral.tariff.hikes","all.tariff.hikes.single.nation","all.tariff.hikes","all.import.restrictions")])

names(figure.4.xlsx)=c("Year", "Total trade affected by US/CHN tariff hikes (incl. TD) targeted at each other", 
                        "Total trade by affected by worldwide tariff hikes (incl.TD) targeted at a single nation",
                        "Total trade by affected by worldwide tariff hikes (incl.TD)",
                        "Total trade by affected by all import barriers worldwide", "US and China bilateral tariff hikes",
                        "All tariff hikes hurting a single nation",
                        "All tariff hikes",
                        "All import distortions")
write.xlsx(figure.4.xlsx, file=paste0(output.path,"fig 4 - table.xlsx"), row.names=F)



figure.4.plot <- gather(figure.4.plot,type, value, 6:9)

# Plot
plot4 <- ggplot()+
  geom_line(data=figure.4.plot, aes(x=year, y=value, colour=forcats::fct_inorder(type)), size=1)+
  geom_text(data=subset(figure.4.plot, type %in% c("all.tariff.hikes","all.import.restrictions")), aes(x=year, y=value, label=value), size=2.5, nudge_y = -0.1)+
  labs(x="Year",y="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)
")+
  scale_color_manual(labels=c("US and China bilateral tariff hikes",
                              "All tariff hikes hurting a single nation",
                              "All tariff hikes",
                              "All import distortions"),
                     values=gta_colour$qualitative)+
  scale_y_continuous(trans="log10", sec.axis = sec_axis(~., name="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)", breaks=c(1,10,100,1000)))+
  scale_x_continuous(breaks=seq(2009,2019,1))+
  guides(colour = guide_legend(title=NULL,ncol = 2))+
  gta_theme()

plot4

gta_plot_saver(plot=plot4,
               path=output.path,
               name="Figure 4",
               eps=F)


# data collection fig 5 ---------------------------------------------------

# F2: For each year 2009-2018, 
# (i) total value of world imports affected by harmful intervention that affects one trading partner, 
# (ii) total value of world imports affected by harmful import distortions that affects any number of trading partners, and 
# (iii) total value of world exports affected by harmful export distortions. 


figure.5=data.frame(year=numeric(),
                     i=numeric(),
                     ii=numeric(),
                     iii=numeric())

for(yr in c(2009:2019)){
  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")
  trade.data=ifelse(yr==2019,yr-2,yr-1)
  
  f32=data.frame(year=yr,
                 i=NA,
                 ii=NA,
                 iii=NA)
  
  ## i
  # (i) total value of world imports affected by harmful intervention that affects one trading partner, 
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     nr.exporters = c(1,1),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f32$i=trade.coverage.estimates[,3]
  f32$i=trade.coverage.estimates[,4]
  
  # (ii) total value of world imports affected by harmful import distortions that affects any number of trading partners, and 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f32$ii=trade.coverage.estimates[,3]
  f32$ii=trade.coverage.estimates[,4]
  
  # (iii) total value of world exports affected by harmful export distortions. 
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "outward subsidy",
                     coverage.period = c(yr, yr),
                     implementation.period = c(yr.start, yr.end),
                     trade.statistic = "value",
                     # trade.data = "before implementation",
                     trade.data = trade.data,
                     intra.year.duration = F
  )
  # f32$iii=trade.coverage.estimates[,3]
  f32$iii=trade.coverage.estimates[,4]
  
  figure.5=rbind(figure.5, f32)
  f32
  rm(f32)
  print(yr)
  
}

# SAVE DATASET
save(figure.5, file=paste0(data.path,"coveragesFigure5.Rdata"))


# plot fig 5 --------------------------------------------------------------

load(paste0(data.path,"coveragesFigure5.Rdata"))

figure.5.0 <- figure.5
figure.5 <- figure.5.0

figure.5$iv <- figure.4$i
figure.5 <- figure.5[,c(1,5,2:4)]
names(figure.5) <- c("year", "i","ii","iii","iv")
figure.5.plot <- figure.5

figure.5.plot$bilateral.tariff.hikes <- 100*(figure.5.plot$i/figure.5.plot[nrow(figure.5.plot),c("i")])
figure.5.plot$all.tariff.hikes.single.nation <- 100*(figure.5.plot$ii/figure.5.plot[nrow(figure.5.plot),c("i")])
figure.5.plot$all.import.restrictions <- 100*(figure.5.plot$iii/figure.5.plot[nrow(figure.5.plot),c("i")])
figure.5.plot$all.export.incentives <- 100*(figure.5.plot$iv/figure.5.plot[nrow(figure.5.plot),c("i")])


figure.5.xlsx=figure.5

figure.5.xlsx <- cbind(figure.5.xlsx, figure.5.plot[,c("bilateral.tariff.hikes","all.tariff.hikes.single.nation","all.import.restrictions","all.export.incentives")])


names(figure.5.xlsx)=c("Year", 
                        "US-China tariff only",
                        "Total value of world imports affected by harmful import barriers that affects one trading partner",
                        "Total value of world imports affected by harmful import barriers that affect any number of trading partners",
                        "Total value of world exports affected by export incentives of competing exporters",
                        "US and China bilateral tariff hikes",
                        "All tariff hikes hurting a single nation",
                        "All export incentives",
                        "All import distortions"
)
write.xlsx(figure.5.xlsx, file=paste0(output.path,"fig 5 - table.xlsx"), row.names=F)


figure.5.plot <- gather(figure.5.plot,type, value, 6:9)

# Plot
plot5 <- ggplot()+
  geom_line(data=figure.5.plot, aes(x=year, y=value, colour=forcats::fct_inorder(type)), size=1)+
  geom_text(data=subset(figure.5.plot, type %in% c("all.export.incentives")), aes(x=year, y=value, label=round(value,0)), size=2.5, nudge_y = 0.15)+
  geom_text(data=subset(figure.5.plot, type %in% c("all.import.restrictions")), aes(x=year, y=value, label=round(value,0)), size=2.5, nudge_y = -0.15)+
  labs(x="Year",y="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)
       ")+
  scale_color_manual(labels=c("US and China bilateral tariff hikes",
                              "All tariff hikes hurting a single nation",
                              "All import distortions",
                              "All export incentives"),
                     values=gta_colour$qualitative)+
  scale_y_continuous(trans="log10",limits = c(0.5,10000),breaks=c(1,10,100,1000,10000), sec.axis = sec_axis(~., name="Total value of trade affected (indexed at \n100 for the total value of trade affected \nby US-China tariff hikes in 2018)", breaks=c(1,10,100,1000,10000)))+
  scale_x_continuous(breaks=seq(2009,2019,1)) +
  guides(colour = guide_legend(title=NULL,ncol = 2))+
  gta_theme()

plot5

gta_plot_saver(plot=plot5,
               path=output.path,
               name="Figure 5",
               eps=F)



