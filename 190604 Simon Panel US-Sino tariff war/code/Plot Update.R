rm(list=ls())

library(gtalibrary)

gta_setwd()

cutoff = "2019-04-15"
project = '4 data queries'
query = '190604 Simon Panel US-Sino tariff war'
output.path = paste(project, query,'output/',sep='/')

gta_colour_palette()

#  us-china -----------------------------------------------------------------------

base=data.frame(administration=c("Obama II","Trump 1st year", "Trump 2nd year","Trump 3rd year"),
                     end.date=c("2017-01-19", "2017-12-31","2018-12-31", cutoff),
                     nr.interventions=NA,
                     trade.value=NA,
                     trade.share=NA)

for(i in 1:nrow(base)){
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(base$end.date[i]),year(base$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(base$end.date[i])),
                     implementers = "United States of America",
                     keep.implementer = T,
                     exporters =  "China",
                     keep.exporters = T,
                     trade.statistic = "value",
                     trade.data = "base",
                     intra.year.duration = F
                     )
  
  master.sliced=subset(master.sliced, i.un==840)
  
  base$nr.interventions[i]=length(unique(master.sliced$intervention.id))
  col.name = names(trade.coverage.estimates)[grep("Trade coverage", colnames(trade.coverage.estimates))]
  base$trade.value[i]=trade.coverage.estimates[,col.name]/3

  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(base$end.date[i]),year(base$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(base$end.date[i])),
                     importers = "United States of America",
                     keep.importers = T,
                     implementers = "United States of America",
                     keep.implementer = T,
                     exporters =  "China",
                     keep.exporters = T,
                     trade.statistic = "share",
                     trade.data = "base",
                     intra.year.duration = F)
  
  base$trade.share[i]=trade.coverage.estimates[,col.name]
  
  rm(master.sliced, trade.coverage.estimates)
}

base <- gather(base, type, value, 4:ncol(base))

corrected.plot <- ggplot()+
  geom_bar(data=subset(base, type == "trade.value"), aes(x=administration, y=value/1e9, fill=type), stat = "identity", width=0.6, fill= gta_colour$blue[1]) +
  geom_line(data=subset(base, type == "trade.share"), aes(x=administration, y=value*300, group=1),colour=gta_colour$qualitative[6],size=1.1)+
  geom_text(data=subset(base, type == "trade.share"), aes(x=administration, y=value*300, label=round(value, digits = 3)), nudge_y = -20, size=3.5, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,500,50), limits = c(0,300),sec.axis = sec_axis(~.*(1/300), name = "Share of Chinese exports\naffected by the US"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018", "2019"))+
  xlab("Period")+
  ylab("Chinese exports affected\n by the US (billion US Dollars)")+
  guides(fill=guide_legend(title="Number of interventions \naffecting trade", ncol=3,title.position = "top"))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12)
  )

corrected.plot

gta_plot_saver(plot=corrected.plot,
               path=output.path,
               name="Chinese exports affected by the US - Comparison of administrations in the US-Sino tariff war (base 2005-2007)")


# china-us ----------------------------------------------------------------


base=data.frame(administration=c("Obama II","Trump 1st year", "Trump 2nd year","Trump 3rd year"),
                end.date=c("2017-01-19", "2017-12-31","2018-12-31", cutoff),
                nr.interventions=NA,
                trade.value=NA,
                trade.share=NA)

for(i in 1:nrow(base)){
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(base$end.date[i]),year(base$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(base$end.date[i])),
                     implementers = "China",
                     keep.implementer = T,
                     exporters =  "United States of America",
                     keep.exporters = T,
                     trade.statistic = "value",
                     trade.data = "base",
                     intra.year.duration = F
  )
  
  master.sliced=subset(master.sliced, i.un==840)
  
  base$nr.interventions[i]=length(unique(master.sliced$intervention.id))
  col.name = names(trade.coverage.estimates)[grep("Trade coverage", colnames(trade.coverage.estimates))]
  base$trade.value[i]=trade.coverage.estimates[,col.name]/3
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(base$end.date[i]),year(base$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(base$end.date[i])),
                     importers = "China",
                     keep.importers = T,
                     implementers = "China",
                     keep.implementer = T,
                     exporters =  "United States of America",
                     keep.exporters = T,
                     trade.statistic = "share",
                     trade.data = "base",
                     intra.year.duration = F)
  
  base$trade.share[i]=trade.coverage.estimates[,col.name]
  
  rm(master.sliced, trade.coverage.estimates)
}

base <- gather(base, type, value, 4:ncol(base))

corrected.plot <- ggplot()+
  geom_bar(data=subset(base, type == "trade.value"), aes(x=administration, y=value/1e9, fill=type), stat = "identity", width=0.6, fill= gta_colour$blue[1]) +
  geom_line(data=subset(base, type == "trade.share"), aes(x=administration, y=value*45, group=1),colour=gta_colour$qualitative[6],size=1.1)+
  geom_text(data=subset(base, type == "trade.share"), aes(x=administration, y=value*45, label=round(value, digits = 3)), nudge_y = -3, size=3.5, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,100,5), limits = c(0,45),sec.axis = sec_axis(~.*(1/45), name = "Share of US exports\naffected by China"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018", "2019"))+
  xlab("Period")+
  ylab("US exports affected\n by China (billion US Dollars)")+
  guides(fill=guide_legend(title="Number of interventions \naffecting trade", ncol=3,title.position = "top"))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12)
  )

corrected.plot

gta_plot_saver(plot=corrected.plot,
               path=output.path,
               name="US exports affected by China - Comparison of administrations in the US-Sino tariff war")

