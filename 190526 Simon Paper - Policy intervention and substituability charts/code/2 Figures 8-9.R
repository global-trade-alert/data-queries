library("gtalibrary")
library("xlsx")
library("tidyverse")
library("splitstackshape")
library("lubridate")
library("ggplot2")
library("ggradar")
library("fmsb")

# devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)

rm(list=ls())

gta_setwd()

# LOAD ALL DEFINITIONS
source("4 data queries/190526 Simon Paper - Policy intervention and substituability charts/code/0 Definitions.R")

# LOAD ALL DATA
load(paste0(data.path,"percentages.Rdata"))


#---------------------------------------------#
#                                             #
#   PLOT 8                                    #
#                                             #
#---------------------------------------------#
# 8. Please produce a spider diagram comparing the G20 and Next 20 
# along the following dimensions: 
# % harmful (separately for all 3 periods)
# % traditional (separately for all 3 periods)
# % subsidy (separately for all 3 periods)


fig8 <- data.percentages

# CALCULATE MEAN FOR G20 AND NEXTG20
fig8.plot <- merge(aggregate(harmful.percentage ~ name + period, fig8, mean),
                   merge(aggregate(traditional.percentage ~ name + period, fig8, mean),
                         aggregate(subsidy.percentage ~ name + period, fig8, mean),
                         by=c("name","period")),
                   by=c("name","period"))

# WRITE EXCEL
write.xlsx(fig8.plot, file=paste0(output.path,"Table for figure 8.xlsx"), sheetName = "Fig8", row.names = F)

for (i in 1:length(periods)) {

    print(paste0("Period: ",i))
      
    plot <- subset(fig8.plot, period == i & name != "all")
    plot$period <- NULL
    
    png(paste0(output.path,"Figure 8 - Period ",i,".png"), width = 5000, height = 3000,res = 600)
    par(mar=c(0,0,0,0))
    ggradar(plot, axis.labels = c("Harmful \nPercentage","Traditional \nPercentage","Subsidy \nPercentage"),
            group.colours = c(gta_colour$qualitative[c(4,1)]),
            axis.label.offset = 1.2,
            grid.label.size = 5,
            gridline.label.offset = -0.1,
            legend.text.size = 12,
            axis.label.size = 5,
            legend.position = c(0.9,0.8))
    dev.off() 

}


#---------------------------------------------#
#                                             #
#   PLOT 9                                    #
#                                             #
#---------------------------------------------#
# 9. Please produce a spider diagram comparing the G20 and Next 20 
# along the following dimensions: 
# % harmful (separately for all 3 periods)
# import share (separately for all 3 periods)
# export share (separately for all 3 periods)


fig9 <- data.percentages

# CALCULATE MEAN FOR G20 AND NEXTG20
# fig9.plot <- merge(aggregate(harmful.percentage ~ name + period, fig9, mean),
#                    merge(aggregate(traditional.percentage ~ name + period, fig9, mean),
#                          aggregate(subsidy.percentage ~ name + period, fig9, mean),
#                          by=c("name","period")),
#                    by=c("name","period"))

# WRITE EXCEL
write.xlsx(fig9.plot, file=paste0(output.path,"Table for figure 9.xlsx"), sheetName = "Fig9", row.names = F)

for (i in 1:length(periods)) {
  
  print(paste0("Period: ",i))
  
  plot <- subset(fig9.plot, period == i & name != "all")
  plot$period <- NULL
  rwnames <- plot[,-1]
  rownames(plot) <- plot[,1]
  plot$name <- NULL
  
  plot <- rbind(data.frame(harmful.percentage = 1,
                           traditional.percentage = 1,
                           subsidy.percentage = 1),
                data.frame(harmful.percentage = 0,
                           traditional.percentage = 0,
                           subsidy.percentage = 0),
                plot)
  
  png(paste0(output.path,"Figure 9 - Period ",i,".png"), width = 5000, height = 3000,res = 600)
  par(mar=c(0,0,0,0))
  colors_border=c( gta_colour$qualitative[1], gta_colour$qualitative[2])
  # colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  radarchart(plot, axistype=1, maxmin = T, 
             pcol=colors_border , pfcol=rgb(0,0,0,0) , plwd=4, plty=1,
             cglcol="grey", cglty=1, axislabcol="#222222", caxislabels=seq(0,1,0.25), cglwd=0.8,
             vlcex=0.8,vlabels = c("Harmful Percentage","Traditional Percentage","Subsidy Percentage"))
  legend(x=0.7, y=1, legend = rownames(plot[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "#222222", cex=1.1, pt.cex=2)
  
  dev.off() 
  
}

