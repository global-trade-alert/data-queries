library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")
library("ggplot2")
library(scales)

rm(list=ls())

gta_setwd()

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")

# LOAD ALL DATA
load(paste0(data.path,"percentages.Rdata"))
load(paste0(data.path,"worldbankdata.Rdata"))
load(paste0(data.path,"shares.Rdata"))



#---------------------------------------------#
#                                             #
#   PLOT 1                                    #
#                                             #
#---------------------------------------------#
# 1. For all three time periods and for all countries and for G20, 
# please plot % traditional against % harmful (6 charts in total)

fig1 <- data.percentages
fig1$subsidy.percentage <- NULL

# WRITE EXCEL
write.xlsx(fig1, file=paste0(output.path,"Table for figure 1.xlsx"), sheetName = "Fig1", row.names = F)

fig1.plot <- rbind(subset(fig1, ! name %in% c("all","nextg20")), subset(fig1, name == "all" & ! i.un %in% g20))

# CREATE R SQUARED VALUE FILE
r.squared <- data.frame()

for (i in 1:length(periods)) {

    print(paste0("Period: ",i))
      
      plot <- ggplot() +
        geom_point(data=subset(fig1.plot, period == i & name != "nextg20"), aes(x=harmful.percentage, y=traditional.percentage, color=name), size = 1.5) +
        scale_x_continuous()+
        scale_y_continuous(sec.axis = dup_axis()) +
        labs(x="Harmful interventions as \nshare of all interventions", y="Tariff increases, MAST chapter D, \nE1, E2, E5, E6 and E9 as share of \nall harmful interventions")+
        scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels=c("Non-G20","G20"))+
        guides(colour = guide_legend(title = "Groups", title.position = "top"))+
        gta_theme()+
        geom_smooth(data=subset(fig1.plot, period == i & name == "all"), aes(x=harmful.percentage, y=traditional.percentage), method = "lm", color = gta_colour$qualitative[4],se = F)+
        geom_smooth(data=subset(fig1.plot, period == i & name == "g20"), aes(x=harmful.percentage, y=traditional.percentage), method = "lm", color = gta_colour$qualitative[1],se = F)
      
      
      plot
      
      gta_plot_saver(plot = plot,
                     path = output.path,
                     name=paste0("Figure 1 - Period ",i),
                     eps = F)

      # CALCULATE R SQUARED VALUES
      lmg20 <- lm(traditional.percentage ~ harmful.percentage, subset(fig1.plot, period == i & name == "g20")) 
      lmnong20 <- lm(traditional.percentage ~ harmful.percentage, subset(fig1.plot, period == i & name == "all")) 
      r.squared <- rbind(r.squared, data.frame(period = i,
                                               g20 = summary(lmg20)$r.squared,
                                               non.g20 = summary(lmnong20)$r.squared,
                                               formula = "traditional ~ harmful"))
      }

# SAVE R SQUARED VALUES
write.xlsx(r.squared, file = paste0(output.path,"R squared.values.xlsx"), sheetName = "Fig1", row.names = F, append = F)

#---------------------------------------------#
#                                             #
#   PLOT 2                                    #
#                                             #
#---------------------------------------------#
# 2. For all three time periods and for all countries 
# and for G20, please plot % subsidy against % harmful (6 charts in total)

fig2 <- data.percentages
fig2$traditional.percentage <- NULL

# WRITE EXCEL
write.xlsx(fig2, file=paste0(output.path,"Table for figure 2.xlsx"), sheetName = "Fig2", row.names = F)

fig2.plot <- rbind(subset(fig2, ! name %in% c("all","nextg20")), subset(fig2, name == "all" & ! i.un %in% g20))

# CREATE R SQUARED VALUE FILE
r.squared <- data.frame()

for (i in 1:length(periods)) {

  print(paste0("Period: ",i))
  
    plot <- ggplot() +
      geom_point(data=subset(fig2.plot, period == i & name != "nextg20"), aes(x=harmful.percentage, y=subsidy.percentage, color=name), size = 1.5) +
      scale_x_continuous()+
      scale_y_continuous(sec.axis = dup_axis()) +
      labs(x="Harmful interventions as \nshare of all interventions", y="MAST chapter L, P7 \nand P8 as share of all \nharmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels = c("Non-G20", "G20"))+
      guides(colour = guide_legend(title = "Groups", title.position = "top"))+
      gta_theme()+
      geom_smooth(data=subset(fig2.plot, period == i & name == "all"), aes(x=harmful.percentage, y=subsidy.percentage), method = "lm", color = gta_colour$qualitative[4],se = F)+
      geom_smooth(data=subset(fig2.plot, period == i & name == "g20"), aes(x=harmful.percentage, y=subsidy.percentage), method = "lm", color = gta_colour$qualitative[1],se = F)
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 2 - Period ",i),
                   eps = F)
    
    # CALCULATE R SQUARED VALUES
    lmg20 <- lm(subsidy.percentage ~ harmful.percentage, subset(fig2.plot, period == i & name == "g20")) 
    lmnong20 <- lm(subsidy.percentage ~ harmful.percentage, subset(fig2.plot, period == i & name == "all")) 
    r.squared <- rbind(r.squared, data.frame(period = i,
                                             g20 = summary(lmg20)$r.squared,
                                             non.g20 = summary(lmnong20)$r.squared,
                                             formula = "subsidy ~ harmful"))
}

# SAVE R SQUARED VALUES
write.xlsx(r.squared, file = paste0(output.path,"R squared.values.xlsx"), sheetName = "Fig2", row.names = F, append = T)


#---------------------------------------------#
#                                             #
#   PLOT 3                                    #
#                                             #
#---------------------------------------------#
# 3. For all three time periods and for all countries and 
# for G20, please plot % import share against % harmful (6 charts in total)

fig3 <- data.percentages
fig3[,c("subsidy.percentage","traditional.percentage")] <- NULL

# APPEND IMPORT SHARES
fig3.temp <- import.share
names(fig3.temp) <- c("name","i.un","share","period")
fig3 <- merge(fig3, fig3.temp[,c("i.un","share","period")], by=c("i.un","period"))
rm(fig3.temp)

# WRITE EXCEL
write.xlsx(fig3, file=paste0(output.path,"Table for figure 3.xlsx"), sheetName = "Fig3", row.names = F)

fig3.plot <- rbind(subset(fig3, ! name %in% c("all","nextg20")), subset(fig3, name == "all" & ! i.un %in% g20))

# CREATE R SQUARED VALUE FILE
r.squared <- data.frame()

for (i in 1:length(periods)) {

  print(paste0("Period: ",i))
  
    plot <- ggplot() +
      geom_point(data=subset(fig3.plot, period == i), aes(x=harmful.percentage, y=share, color=name), size = 1.5) +
      scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25), sec.axis = dup_axis()) +
      labs(x="Harmful interventions as \nshare of all interventions", y="Share of imports affected by \ninterventions implemented")+
      scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels = c("Non-G20", "G20"))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()+
      geom_smooth(data=subset(fig3.plot, period == i & name == "all"), aes(x=harmful.percentage, y=share), method = "lm", color = gta_colour$qualitative[4],se = F)+
      geom_smooth(data=subset(fig3.plot, period == i & name == "g20"), aes(x=harmful.percentage, y=share), method = "lm", color = gta_colour$qualitative[1],se = F)
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 3 - Period ",i),
                   eps = F)
    
    # CALCULATE R SQUARED VALUES
    lmg20 <- lm(share ~ harmful.percentage, subset(fig3.plot, period == i & name == "g20")) 
    lmnong20 <- lm(share ~ harmful.percentage, subset(fig3.plot, period == i & name == "all")) 
    r.squared <- rbind(r.squared, data.frame(period = i,
                                             g20 = summary(lmg20)$r.squared,
                                             non.g20 = summary(lmnong20)$r.squared,
                                             formula = "import share ~ harmful"))
}

# SAVE R SQUARED VALUES
write.xlsx(r.squared, file = paste0(output.path,"R squared.values.xlsx"), sheetName = "Fig3", row.names = F, append = T)




#---------------------------------------------#
#                                             #
#   PLOT 4                                    #
#                                             #
#---------------------------------------------#
# 4. For all three time periods and for all countries and 
# for G20, please plot % export share against % harmful (6 charts in total)

# fig4 <- data.percentages
# fig4[,c("subsidy.percentage","traditional.percentage")] <- NULL
# 
# # APPEND IMPORT SHARES
# fig4.temp <- export.share
# names(fig4.temp) <- c("name","i.un","share","period")
# fig4 <- merge(fig4, fig4.temp[,c("i.un","share","period")], by=c("i.un","period"))
# rm(fig4.temp)
# 
# # WRITE EXCEL
# write.xlsx(fig4, file=paste0(output.path,"Table for figure 4.xlsx"), sheetName = "Fig4", row.names = F)
# 
# fig4.plot <- rbind(subset(fig4, ! name %in% c("all","nextg20")), subset(fig4, name == "all" & ! i.un %in% g20))
# 
# for (i in 1:length(periods)) {
#   
#   print(paste0("Period: ",i," / Group: ",g))
#   
#   plot <- ggplot() +
#     geom_point(data=subset(fig4.plot, period == i), aes(x=harmful.percentage, y=share, color=name), size = 1.5) +
#     scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
#     scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25), sec.axis = dup_axis()) +
#     labs(x="Harmful interventions as \nshare of all interventions", y="Share of exports affected by \ninterventions implemented")+
#     scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels = c("Non-G20", "G20"))+
#     guides(colour = guide_legend(title = "Shares", title.position = "top"))+
#     gta_theme()
#   
#   plot
#   
#   gta_plot_saver(plot = plot,
#                  path = output.path,
#                  name=paste0("Figure 4 - Period ",i))
# }


#---------------------------------------------#
#                                             #
#   PLOT 5                                    #
#                                             #
#---------------------------------------------#
# 5. For all three time periods and for all countries and for G20, 
# please plot real government spending increase (over the time 
# period in question) against % harmful (6 charts in total).

fig5 <- data.percentages
fig5[,c("subsidy.percentage","traditional.percentage")] <- NULL

# APPEND REAL GOVERNMENT SPENDING COL
fig5.temp <- gov.spending
names(fig5.temp) <- c("i.un","name","period","start","end","growth.rate")
fig5 <- merge(fig5, fig5.temp[,c("i.un","growth.rate","period")], by=c("i.un","period"))
rm(fig5.temp)

# WRITE EXCEL
write.xlsx(fig5, file=paste0(output.path,"Table for figure 5.xlsx"), sheetName = "Fig5", row.names = F)

fig5.plot <- rbind(subset(fig5, ! name %in% c("all","nextg20")), subset(fig5, name == "all" & ! i.un %in% g20))

# CREATE R SQUARED VALUE FILE
r.squared <- data.frame()

for (i in 1:length(periods)) {

  print(paste0("Period: ",i))
  
    plot <- ggplot() +
      geom_point(data=subset(fig5.plot, (period == i)), aes(x=harmful.percentage, y=growth.rate, color=name), size = 1.5) +
      scale_x_continuous()+
      scale_y_continuous(breaks = seq(-1,1,0.25), limits = c(-1,1), sec.axis = dup_axis()) +
      labs(x="Harmful interventions as \nshare of all interventions", y="Growth real government spending")+
      scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels = c("Non-G20", "G20"))+
      guides(colour = guide_legend(title = "Groups", title.position = "top"))+
      gta_theme()+
      geom_smooth(data=subset(fig5.plot, period == i & name == "all"), aes(x=harmful.percentage, y=growth.rate), method = "lm", color = gta_colour$qualitative[4],se = F)+
      geom_smooth(data=subset(fig5.plot, period == i & name == "g20"), aes(x=harmful.percentage, y=growth.rate), method = "lm", color = gta_colour$qualitative[1],se = F)
    
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 5 - Period ",i),
                   eps = F)
    
    # CALCULATE R SQUARED VALUES
    lmg20 <- lm(growth.rate ~ harmful.percentage, subset(fig5.plot, period == i & name == "g20")) 
    lmnong20 <- lm(growth.rate ~ harmful.percentage, subset(fig5.plot, period == i & name == "all")) 
    r.squared <- rbind(r.squared, data.frame(period = i,
                                             g20 = summary(lmg20)$r.squared,
                                             non.g20 = summary(lmnong20)$r.squared,
                                             formula = "growth of government expenditures ~ harmful"))
}

# SAVE R SQUARED VALUES
write.xlsx(r.squared, file = paste0(output.path,"R squared.values.xlsx"), sheetName = "Fig5", row.names = F, append = T)



#---------------------------------------------#
#                                             #
#   PLOT 6                                    #
#                                             #
#---------------------------------------------#
# 6. For all three time periods and for all countries and for G20, please plot 
# devaluation against the US Dollar (over the time period in question) against % harmful??(6 charts in total).

fig6 <- data.percentages
fig6[,c("subsidy.percentage","traditional.percentage")] <- NULL

# APPEND DEVALUATION AGAINST US DOLLAR COL
fig6.temp <- exch.rate
names(fig6.temp) <- c("i.un","name","period","start","end","growth.rate")
fig6 <- merge(fig6, fig6.temp[,c("i.un","growth.rate","period")], by=c("i.un","period"))
rm(fig6.temp)

# WRITE EXCEL
write.xlsx(fig6, file=paste0(output.path,"Table for figure 6.xlsx"), sheetName = "Fig6", row.names = F)

fig6.plot <- rbind(subset(fig6, ! name %in% c("all","nextg20")), subset(fig6, name == "all" & ! i.un %in% g20))

# CREATE R SQUARED VALUE FILE
r.squared <- data.frame()

for (i in 1:length(periods)) {

  print(paste0("Period: ",i))
  
    plot <- ggplot() +
      geom_point(data=subset(fig6.plot, (period == i)), aes(x=harmful.percentage, y=growth.rate, color=name), size = 1.5) +
      scale_x_continuous()+
      scale_y_continuous(breaks = seq(-1,3,0.25), limits = c(-1,3), sec.axis = dup_axis(), labels=percent) +
      labs(x="Harmful interventions as \nshare of all interventions", y="Gain of LCU against US Dollar")+
      scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels = c("Non-G20", "G20"))+
      guides(colour = guide_legend(title = "Groups", title.position = "top"))+
      gta_theme()+
      geom_smooth(data=subset(fig6.plot, period == i & name == "all"), aes(x=harmful.percentage, y=growth.rate), method = "lm", color = gta_colour$qualitative[4],se = F)+
      geom_smooth(data=subset(fig6.plot, period == i & name == "g20"), aes(x=harmful.percentage, y=growth.rate), method = "lm", color = gta_colour$qualitative[1],se = F)
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 6 - Period ",i),
                   eps = F)
    
    # CALCULATE R SQUARED VALUES
    lmg20 <- lm(growth.rate ~ harmful.percentage, subset(fig6.plot, period == i & name == "g20")) 
    lmnong20 <- lm(growth.rate ~ harmful.percentage, subset(fig6.plot, period == i & name == "all")) 
    r.squared <- rbind(r.squared, data.frame(period = i,
                                             g20 = summary(lmg20)$r.squared,
                                             non.g20 = summary(lmnong20)$r.squared,
                                             formula = "growth of devaluation against us dollar ~ harmful"))
}

# SAVE R SQUARED VALUES
write.xlsx(r.squared, file = paste0(output.path,"R squared.values.xlsx"), sheetName = "Fig6", row.names = F, append = T)



#---------------------------------------------#
#                                             #
#   PLOT 7                                    #
#                                             #
#---------------------------------------------#
# 7. For all three time periods for all countries and for G20, 
# please plot unemployment rate increase (2007-2009) against
# % harmful??(6 charts in total).

fig7 <- data.percentages
fig7[,c("subsidy.percentage","traditional.percentage")] <- NULL

# APPEND DEVALUATION AGAINST US DOLLAR COL
fig7.temp <- un.rate
names(fig7.temp) <- c("i.un","name","period","start","end","growth.rate")
fig7 <- merge(fig7, fig7.temp[,c("i.un","growth.rate","period")], by=c("i.un","period"))
rm(fig7.temp)

# WRITE EXCEL
write.xlsx(fig7, file=paste0(output.path,"Table for figure 7.xlsx"), sheetName = "Fig7", row.names = F)

fig7.plot <- rbind(subset(fig7, ! name %in% c("all","nextg20")), subset(fig7, name == "all" & ! i.un %in% g20))

# CREATE R SQUARED VALUE FILE
r.squared <- data.frame()

for (i in 1:length(periods)) {

  print(paste0("Period: ",i))
  
    plot <- ggplot() +
      geom_point(data=subset(fig7.plot, (period == i)), aes(x=harmful.percentage, y=growth.rate, color=name), size = 1.5) +
      scale_x_continuous()+
      scale_y_continuous(breaks = seq(-1,3.2,0.25), limits = c(-1,3.2), sec.axis = dup_axis()) +
      labs(x="Harmful interventions as \nshare of all interventions", y="Growth of unemployment rate")+
      scale_color_manual(values = gta_colour$qualitative[c(4,1)], labels = c("Non-G20", "G20"))+
      guides(colour = guide_legend(title = "Groups", title.position = "top"))+
      gta_theme()+
      geom_smooth(data=subset(fig7.plot, period == i & name == "all"), aes(x=harmful.percentage, y=growth.rate), method = "lm", color = gta_colour$qualitative[4],se = F)+
      geom_smooth(data=subset(fig7.plot, period == i & name == "g20"), aes(x=harmful.percentage, y=growth.rate), method = "lm", color = gta_colour$qualitative[1],se = F)
    
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 7 - Period ",i),
                   eps = F)
    
    # CALCULATE R SQUARED VALUES
    lmg20 <- lm(growth.rate ~ harmful.percentage, subset(fig7.plot, period == i & name == "g20")) 
    lmnong20 <- lm(growth.rate ~ harmful.percentage, subset(fig7.plot, period == i & name == "all")) 
    r.squared <- rbind(r.squared, data.frame(period = i,
                                             g20 = summary(lmg20)$r.squared,
                                             non.g20 = summary(lmnong20)$r.squared,
                                             formula = "growth of unemployment rate ~ harmful"))

}

# SAVE R SQUARED VALUES
write.xlsx(r.squared, file = paste0(output.path,"R squared.values.xlsx"), sheetName = "Fig7", row.names = F, append = T)

