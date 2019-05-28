library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")
library("ggplot2")

rm(list=ls())

setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")

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
fig1 <- gather(fig1, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))
fig1 <- subset(fig1, type != "subsidy.percentage")

# WRITE EXCEL
write.xlsx(fig1, file=paste0(output.path,"Table for figure 1.xlsx"), sheetName = "Fig1", row.names = F)

# ADD ORDER COLUMN
fig1$order[fig1$type == "harmful.percentage"] <- 1
fig1$order[fig1$type == "subsidy.percentage"] <- 2
fig1 <- fig1[with(fig1, order(order)),]
row.names(fig1) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
      
      plot <- ggplot() +
        geom_line(data=subset(fig1, period == paste0("period.",i) & name == groups.name[g] & type != "subsidy.percentage"), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
        scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
        scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), sec.axis = dup_axis()) +
        labs(x="Year", y="Share of all harmful interventions")+
        scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", "Tariff increases, MAST chapter D, \nE1, E2, E5, E6, and E9 as share of \nall harmful interventions"))+
        guides(colour = guide_legend(title = "Shares", title.position = "top"))+
        gta_theme()
      
      plot
      
      gta_plot_saver(plot = plot,
                     path = output.path,
                     name=paste0("Figure 1 - Period ",i," - ",groups.name[g]))
  }
}


#---------------------------------------------#
#                                             #
#   PLOT 2                                    #
#                                             #
#---------------------------------------------#
# 2. For all three time periods and for all countries 
# and for G20, please plot % subsidy against % harmful (6 charts in total)


fig2 <- data.percentages
fig2 <- gather(fig2, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))
fig2 <- subset(fig2, type != "traditional.percentage")

# WRITE EXCEL
write.xlsx(fig2, file=paste0(output.path,"Table for figure 2.xlsx"), sheetName = "Fig2", row.names = F)

# ADD ORDER COLUMN
fig2$order[fig2$type == "harmful.percentage"] <- 1
fig2$order[fig2$type == "subsidy.percentage"] <- 2
fig2 <- fig2[with(fig2, order(order)),]
row.names(fig2) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
    
    plot <- ggplot() +
      geom_line(data=subset(fig2, period == paste0("period.",i) & name == groups.name[g]), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
      scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), sec.axis = dup_axis()) +
      labs(x="Year", y="Share of all harmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", "MAST chapter L, P7 \nand P8 as share of all \nharmful interventions"))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 2 - Period ",i," - ",groups.name[g]))
  }
}


#---------------------------------------------#
#                                             #
#   PLOT 3                                    #
#                                             #
#---------------------------------------------#
# 3. For all three time periods and for all countries and 
# for G20, please plot % import share against % harmful (6 charts in total)

fig3 <- data.percentages
fig3 <- gather(fig3, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))

# APPEND IMPORT SHARES
fig3.temp <- import.share
names(fig3.temp) <- c("name","value","period","year")
fig3.temp$type <- "import.shares"
fig3 <- rbind(fig3, fig3.temp)
rm(fig3.temp)

# PREPARE FIG3 SET
fig3 <- subset(fig3, type %in% c("import.shares", "harmful.percentage"))
fig3$year <- as.numeric(as.character(fig3$year))

# WRITE EXCEL
write.xlsx(fig3, file=paste0(output.path,"Table for figure 3.xlsx"), sheetName = "Fig3", row.names = F)

# ADD ORDER COLUMN
fig3$order[fig3$type == "harmful.percentage"] <- 1
fig3$order[fig3$type == "import.shares"] <- 2
fig3 <- fig3[with(fig3, order(order)),]
row.names(fig3) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
    
    plot <- ggplot() +
      geom_line(data=subset(fig3, period == paste0("period.",i) & name == groups.name[g]), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
      scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), sec.axis = dup_axis()) +
      labs(x="Year", y="Share of all harmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", paste0("Share of imports affected by \ninterventions implemented by ",groups.name[g])))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 3 - Period ",i," - ",groups.name[g]))
  }
}


#---------------------------------------------#
#                                             #
#   PLOT 4                                    #
#                                             #
#---------------------------------------------#
# 4. For all three time periods and for all countries and 
# for G20, please plot % export share against % harmful (6 charts in total)

fig4 <- data.percentages
fig4 <- gather(fig4, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))

# APPEND IMPORT SHARES
fig4.temp <- export.share
names(fig4.temp) <- c("name","value","period","year")
fig4.temp$type <- "export.shares"
fig4 <- rbind(fig4, fig4.temp)
rm(fig4.temp)

# PREPARE FIG4 SET
fig4 <- subset(fig4, type %in% c("export.shares", "harmful.percentage"))
fig4$year <- as.numeric(as.character(fig4$year))

# WRITE EXCEL
write.xlsx(fig4, file=paste0(output.path,"Table for figure 4.xlsx"), sheetName = "Fig4", row.names = F)

# ADD ORDER COLUMN
fig4$order[fig4$type == "harmful.percentage"] <- 1
fig4$order[fig4$type == "export.shares"] <- 2
fig4 <- fig4[with(fig4, order(order)),]
row.names(fig4) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
    
    plot <- ggplot() +
      geom_line(data=subset(fig4, period == paste0("period.",i) & name == groups.name[g]), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
      scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), sec.axis = dup_axis()) +
      labs(x="Year", y="Share of all harmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", paste0("Share of exports affected by \ninterventions implemented by ",groups.name[g])))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 4 - Period ",i," - ",groups.name[g]))
  }
}


#---------------------------------------------#
#                                             #
#   PLOT 5                                    #
#                                             #
#---------------------------------------------#
# 5. For all three time periods and for all countries and for G20, 
# please plot real government spending increase (over the time 
# period in question) against % harmful (6 charts in total).

fig5 <- data.percentages
fig5 <- gather(fig5, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))

# APPEND IMPORT SHARES
fig5.temp <- gov.spending
names(fig5.temp) <- c("name","value.gov.spending","year","value")
fig5.temp$period <- "all.periods"
fig5.temp$type <- "gov.spending"
fig5 <- rbind(fig5, fig5.temp[,c("name","value","year","period","type")])
rm(fig5.temp)

# PREPARE FIG5 SET
fig5 <- subset(fig5, type %in% c("harmful.percentage","gov.spending"))
fig5$year <- as.numeric(as.character(fig5$year))

# WRITE EXCEL
write.xlsx(fig5, file=paste0(output.path,"Table for figure 5.xlsx"), sheetName = "Fig5", row.names = F)

# ADD ORDER COLUMN
fig5$order[fig5$type == "harmful.percentage"] <- 1
fig5$order[fig5$type == "gov.spending"] <- 2
fig5 <- fig5[with(fig5, order(order)),]
row.names(fig5) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
    
    plot <- ggplot() +
      geom_line(data=subset(fig5, (period == paste0("period.",i) | period == "all.periods") & name == groups.name[g]), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
      scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1), sec.axis = dup_axis()) +
      labs(x="Year", y="Share of all harmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", paste0("Growth real government \nspending by ",groups.name[g])))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 5 - Period ",i," - ",groups.name[g]))
  }
}


#---------------------------------------------#
#                                             #
#   PLOT 6                                    #
#                                             #
#---------------------------------------------#
# 6. For all three time periods and for all countries and for G20, please plot 
# devaluation against the US Dollar (over the time period in question) against % harmful (6 charts in total).

fig6 <- data.percentages
fig6 <- gather(fig6, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))

# APPEND IMPORT SHARES
fig6.temp <- exch.rate
names(fig6.temp) <- c("name","value","year")
fig6.temp$period <- "all.periods"
fig6.temp$type <- "exchange.rate.growth"
fig6 <- rbind(fig6, fig6.temp[,c("name","value","year","period","type")])
rm(fig6.temp)

# PREPARE FIG6 SET
fig6 <- subset(fig6, type %in% c("harmful.percentage","exchange.rate.growth"))
fig6$year <- as.numeric(as.character(fig6$year))

# WRITE EXCEL
write.xlsx(fig6, file=paste0(output.path,"Table for figure 6.xlsx"), sheetName = "Fig6", row.names = F)

# ADD ORDER COLUMN
fig6$order[fig6$type == "harmful.percentage"] <- 1
fig6$order[fig6$type == "exchange.rate.growth"] <- 2
fig6 <- fig6[with(fig6, order(order)),]
row.names(fig6) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
    
    plot <- ggplot() +
      geom_line(data=subset(fig6, (period == paste0("period.",i) | period == "all.periods") & name == groups.name[g]), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
      scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(-0.1,1), sec.axis = dup_axis()) +
      labs(x="Year", y="Share of all harmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", paste0("Growth devaluation against US Dollar ")))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 6 - Period ",i," - ",groups.name[g]))
  }
}


#---------------------------------------------#
#                                             #
#   PLOT 7                                    #
#                                             #
#---------------------------------------------#
# 7. For all three time periods for all countries and for G20, 
# please plot unemployment rate increase (2007-2009) against
# % harmful (6 charts in total).

fig7 <- data.percentages
fig7 <- gather(fig7, type, value, c("harmful.percentage","traditional.percentage","subsidy.percentage"))

# APPEND IMPORT SHARES
fig7.temp <- un.rate
names(fig7.temp) <- c("name","value","year")
fig7.temp$period <- "all.periods"
fig7.temp$type <- "unemployment.rate"
fig7 <- rbind(fig7, fig7.temp[,c("name","value","year","period","type")])
rm(fig7.temp)

# PREPARE FIG7 SET
fig7 <- subset(fig7, type %in% c("harmful.percentage","unemployment.rate"))
fig7$year <- as.numeric(as.character(fig7$year))

# WRITE EXCEL
write.xlsx(fig7, file=paste0(output.path,"Table for figure 7.xlsx"), sheetName = "Fig7", row.names = F)

# ADD ORDER COLUMN
fig7$order[fig7$type == "harmful.percentage"] <- 1
fig7$order[fig7$type == "unemployment.rate"] <- 2
fig7 <- fig7[with(fig7, order(order)),]
row.names(fig7) <- NULL

for (i in 1:length(periods)) {
  for(g in 1:(length(groups)-1)) {
    
    print(paste0("Period: ",i," / Group: ",g))
    
    plot <- ggplot() +
      geom_line(data=subset(fig7, (period == paste0("period.",i) | period == "all.periods") & name == groups.name[g]), aes(x=year, y=value, color=forcats::fct_inorder(type)), size = 1) +
      scale_x_continuous(breaks = c(year(periods[[i]][1]):year(periods[[i]][2])), limits = c(year(periods[[i]][1]),year(periods[[i]][2])))+
      scale_y_continuous(breaks = seq(0,1,0.1), limits = c(-0.2,1), sec.axis = dup_axis()) +
      labs(x="Year", y="Share of all harmful interventions")+
      scale_color_manual(values = gta_colour$qualitative[c(1,2)], labels = c("Harmful interventions as \nshare of all interventions", paste0("Growth of unemployment rate for ",groups.name[g])))+
      guides(colour = guide_legend(title = "Shares", title.position = "top"))+
      gta_theme()
    
    plot
    
    gta_plot_saver(plot = plot,
                   path = output.path,
                   name=paste0("Figure 7 - Period ",i," - ",groups.name[g]))
  }
}
