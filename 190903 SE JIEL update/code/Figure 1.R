rm(list=ls())

library(gtalibrary)
library(ggplot2)

#Changes made: 
#removal trump tweets
#copied previous code and changed 2018's to 2019's
#important: am i supposed to double the bilateral trade rhs bar now that we talk over 2 years? (line 235)

query.path='4 data queries/190903 SE JIEL update/'
output.path=paste0(query.path,'new output/')
  
gta_setwd()
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

# Please calculate for the latest year available the % of world trade associated with Sino US bilateral trade.
gta_trade_value_bilateral(trade.data = "2017")
total.trade=sum(trade.base.bilateral$trade.value)
chn.us=sum(subset(trade.base.bilateral, i.un==156 & a.un==840)$trade.value)
us.chn=sum(subset(trade.base.bilateral, i.un==840 & a.un==156)$trade.value)

# The fourth column from the left include add our estimates of the amounts of trade affected 
# by non-targeted-tariff increases for 2018 on top of the Chinese and US tariff increases in 2018.

gta_trade_coverage(coverage.period=c(2018,2018),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = T,
                   implementation.period = c("2018-01-01","2018-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(2,999))

other.stuff.18=sum(trade.coverage.estimates[,4])
us.stuff=trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

gta_trade_coverage(coverage.period=c(2018,2018),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = F,
                   implementation.period = c("2018-01-01","2018-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(2,999))

other.stuff.18=other.stuff.18+sum(trade.coverage.estimates[,4])
us.stuff=us.stuff+trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

gta_trade_coverage(coverage.period=c(2018,2018),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = F,
                   implementation.period = c("2018-01-01","2018-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(1,1))

other.stuff.18=other.stuff.18+sum(trade.coverage.estimates[,4])
us.stuff=us.stuff+trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

# Then the first left hand column would have the Trump tariff increases of 2018, 
gta_trade_coverage(coverage.period=c(2018,2018),
                   intervention.ids = trade.war.us,
                   keep.interventions = T,
                   exporters = "China",
                   keep.exporters = T,
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F)

us.2018=trade.coverage.estimates[,4]



# second the next column on the left would add the Chinese tariff increases and 
gta_trade_coverage(coverage.period=c(2018,2018),
                   intervention.ids = trade.war.chn,
                   keep.interventions = T,
                   exporters = "United States of America",
                   keep.exporters = T,
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F)

chn.2018=trade.coverage.estimates[,4]



# recalculate but for 2019 ---------------------------------------
# I Left trade base 2017 for comparability

# The fourth column from the left include add our estimates of the amounts of trade affected 
# by non-targeted-tariff increases for 2019 on top of the Chinese and US tariff increases in 2019

gta_trade_coverage(coverage.period=c(2019,2019),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = T,
                   implementation.period = c("2019-01-01","2019-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(2,999))

other.stuff.19=sum(trade.coverage.estimates[,4])
us.stuff.19=trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

gta_trade_coverage(coverage.period=c(2019,2019),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = F,
                   implementation.period = c("2019-01-01","2019-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(2,999))

other.stuff.19=other.stuff.19+sum(trade.coverage.estimates[,4])
us.stuff.19=us.stuff.19+trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

gta_trade_coverage(coverage.period=c(2019,2019),
                   gta.evaluation = c("Red", "Amber"),
                   affected.flows = "inward",
                   implementers=c("China","United States of America"),
                   keep.implementer = T,
                   exporters = c("China","United States of America"),
                   keep.exporters = T,
                   group.exporters = F,
                   intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                   keep.type = F,
                   implementation.period = c("2019-01-01","2019-12-31"),
                   trade.data="2017",
                   trade.statistic = "value",
                   intra.year.duration = F,
                   nr.exporters = c(1,1))

other.stuff.19=other.stuff.19+sum(trade.coverage.estimates[,4])
us.stuff.19=us.stuff.19+trade.coverage.estimates[which(trade.coverage.estimates$`Exporting country`=="China"),4]

# Then the first left hand column would have the Trump tariff increases of 2019, 
gta_trade_coverage(coverage.period=c(2019,2019),
                   intervention.ids = trade.war.us,
                   keep.interventions = T,
                   exporters = "China",
                   keep.exporters = T,
                   trade.data="2017",
                   trade.statistic = "value",
                   implementation.period = c("2019-01-01","2019-12-31"),
                   intra.year.duration = F)

us.2019=trade.coverage.estimates[,4]



# second the next column on the left would add the Chinese tariff increases and 
gta_trade_coverage(coverage.period=c(2019,2019),
                   intervention.ids = trade.war.chn,
                   keep.interventions = T,
                   exporters = "United States of America",
                   keep.exporters = T,
                   trade.data="2017",
                   trade.statistic = "value",
                   implementation.period = c("2019-01-01","2019-12-31"),
                   intra.year.duration = F)

chn.2019=trade.coverage.estimates[,4]




## total bilateral trade

war.perspective=data.frame(act.title="US\ntariffs\n2018",
                           value=us.2018)

war.perspective=rbind(war.perspective,
                      data.frame(act.title="China\ntariffs\n2018",
                                 value=chn.2018))

war.perspective=rbind(war.perspective,
                      data.frame(act.title="Other\nUS/CHN\ninterventions\nin 2018",
                                 value=other.stuff.18)
)

war.perspective=rbind(war.perspective,
                      data.frame(act.title="US\ntariffs\n2019",
                                 value=us.2019))

war.perspective=rbind(war.perspective,
                      data.frame(act.title="China\ntariffs\n2019",
                                 value=chn.2019))

war.perspective=rbind(war.perspective,
                      data.frame(act.title="Other\nUS/CHN\ninterventions\nin 2019",
                                 value=other.stuff.19)
)

# Changed from JF code: 
# Do i need to double the bilateral trade column since it's now over 2 years or is this not representative anymore?

war.perspective=rbind(war.perspective,
                      data.frame(act.title="Total\nbilateral\ntrade",
                                 value=chn.us*2+us.chn*2))



## exciting waterfall version
## based on https://learnr.wordpress.com/2010/05/10/ggplot2-waterfall-charts/
war.terfall=war.perspective
war.terfall$value=round(war.terfall$value/1000000000,2)
war.terfall$act.title=factor(war.terfall$act.title, levels=war.terfall$act.title)
war.terfall$id=1:nrow(war.terfall)
war.terfall$end=cumsum(war.terfall$value)
war.terfall$end[nrow(war.terfall)]=war.terfall$value[nrow(war.terfall)]
war.terfall$value[nrow(war.terfall)]=war.terfall$end[nrow(war.terfall)]-war.terfall$end[nrow(war.terfall)-1]
war.terfall$start <- c(0, head(war.terfall$end, -1))

war.terfall=war.terfall[,c(3,1,5,4,2)]

war.terfall.xlsx=war.terfall
names(war.terfall.xlsx)=c("ID", "Action", "Starting Value", "Ending value","Change")
openxlsx::write.xlsx(war.terfall.xlsx, file=paste(output.path, "/fig 1 - Trade war interventions in bilateral perspective.xlsx",sep=""), row.names=F)

gta_colour_palette()
#### classic waterfall
# warter.shed=ggplot(war.terfall, aes(act.title, fill = act.title)) + 
#   geom_rect(aes(x = act.title, 
#                 xmin = id - 0.45, xmax = id + 0.45, 
#                 ymin = end, ymax = start))+
#   scale_fill_manual(values=c(gta_colour$qualitative[c(1,7,5,3,8,4,2)]))+
#   scale_y_continuous(limit=c(0,1400),breaks=seq(0,1400,100), sec.axis = dup_axis())+
#   labs(x="", y="USD billion", color="")+
#   gta_theme(x.bottom.angle = 0)+
#   theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
#         axis.text.y.left = element_text(size=18),
#         axis.text.y.right = element_text(size=18),
#         axis.title.y.left = element_text(size=16),
#         axis.title.y.right = element_text(size=16),
#         axis.title.x.bottom = element_text(size=18),
#         legend.position="none")
# 
# gta_plot_saver(plot=warter.shed,
#                path=output.path,
#                name="fig 1 - Trade war in bilateral perspective - waterfall chart",
#                eps=F)

### with full right-hand bar
war.terfall$start[nrow(war.terfall)]=0

warter.shed=ggplot(war.terfall, aes(act.title, fill = act.title)) + 
  geom_rect(aes(x = act.title, 
                xmin = id - 0.45, xmax = id + 0.45, 
                ymin = end, ymax = start))+
  scale_fill_manual(values=c(gta_colour$qualitative[c(1,7,5,3,8,4,2)]))+
  scale_y_continuous(limit=c(0,1400),breaks=seq(0,1400,100), sec.axis = dup_axis())+
  labs(x="", y="USD billion", color="")+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(vjust = 0.5, size=13),
        axis.text.y.left = element_text(size=18),
        axis.text.y.right = element_text(size=18),
        axis.title.y.left = element_text(size=16),
        axis.title.y.right = element_text(size=16),
        axis.title.x.bottom = element_text(size=18),
        legend.position="none")

gta_plot_saver(plot=warter.shed,
               path=output.path,
               name="fig 1 - Trade war in bilateral perspective - waterfall chart with full rhs bar",
               eps=F)


## boring bar plot version
# war.picture=ggplot(war.perspective, aes(x = factor(act.title), y = round(value/1000000000,0), fill=act.title)) +
#   geom_col() + 
#   geom_text(aes(y = round(value/1000000000,0) + 30, label = round(value/1000000000,0)), size=5,position = position_dodge(w = -0.5))+
#   scale_fill_manual(values=c(gta_colour$qualitative[c(1,7,5,3,8,4,2)]))+
#   scale_y_continuous(limit=c(0,750),breaks=seq(0,700,100), sec.axis = dup_axis())+
#   labs(x="", y="USD billion", color="")+
#   gta_theme(x.bottom.angle = 0)+
#   theme(axis.text.x.bottom = element_text(vjust = 0.5, size=18),
#         axis.text.y.left = element_text(size=18),
#         axis.text.y.right = element_text(size=18),
#         axis.title.y.left = element_text(size=16),
#         axis.title.y.right = element_text(size=16),
#         axis.title.x.bottom = element_text(size=18),
#         legend.position="none")
# 
# 
# gta_plot_saver(plot=war.picture,
#                path=output.path,
#                name="fig 1 - Trade war interventions in US-CHN bilateral perspective",
#                eps=F)
