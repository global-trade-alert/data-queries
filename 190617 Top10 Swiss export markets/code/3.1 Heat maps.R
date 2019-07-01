library("gtalibrary")
library("xlsx")
library("tidyverse")
library("WDI")

rm(list=ls())

gta_setwd()

source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")
gta_colour_palette()

# GET TOP MARKETS AND SECTORS
load(paste0(data.path,"Top CPC and markets.Rdata"))
top.cpc <- top.cpc$cpc
top.markets <- top.markets$un_code

load(paste0(data.path,"results matrix.Rdata"))

# PRETTIFY CPC NAMES
cpc.names <- gtalibrary::cpc.names
results <- merge(results, cpc.names[c("cpc","cpc.name")], by = "cpc")

results$cpc.name <- as.character(results$cpc.name)

results$cpc.name[results$cpc.name == "Basic organic chemicals"] <- "Basic organic chemicals"
results$cpc.name[results$cpc.name == "Electricity distribution & control apparatus; parts"] <- "Electricity apparatus"
results$cpc.name[results$cpc.name == "Food products n.e.c."] <- "Food products"
results$cpc.name[results$cpc.name == "Instruments & control equipment, except optical instruments"] <- "Instruments equipment"
results$cpc.name[results$cpc.name == "Jewellery & related articles"] <- "Jewellery"
results$cpc.name[results$cpc.name == "Machine-tools & parts"] <- "Machine-tools"
results$cpc.name[results$cpc.name == "Medical & surgical equipment & orthopaedic appliances"] <- "Medical equipment"
results$cpc.name[results$cpc.name == "Other special-purpose machinery & parts"] <- "Special-purpose machinery"
results$cpc.name[results$cpc.name == "Pharmaceutical products"] <- "Pharmaceutical products"
results$cpc.name[results$cpc.name == "Watches & clocks; parts"] <- "Watches & clocks"
results$name[results$name == "United Kingdom"] <- "UK"
results$name[results$name == "United States of America"] <- "USA"

results$cpc.name <- paste0(results$cpc.name," (", results$cpc, ")")

results <- results[,c("cpc.name","name","distortions.2017","distortions.2017.hit3orMore","export.incentives.2017","export.incentives.2017.hit3orMore")]

write.xlsx(results, file=paste0(output.path,"Table for figure 8-11.xlsx"),sheetName="Results",row.names=F)


# MAKE HEAT MAP FOR DISTORTIONS 2017 AND VARIANT, AND EXPORT INCENTIVES 2017 AND VARIANT

# ADD NUMERIC COLUMNS FOR EASIER PLOTTING
results$x.num <- rep(seq(1,10,1),1)
results$y.num <- rep(seq(1,10,1),each=10)

cty.names <- unique(results$name)
cpc.names <- unique(results$cpc.name)

heatmap <- function(x.axis = NULL,
                        y.axis = NULL,
                    value = NULL,
                        output.name = NULL,
                        labs = c("x","y")) 
{
  
  
  master <- results[,c(x.axis,y.axis,value)]
  names(master) <- c("x","y","value")
  master$rank <- 1
  master$rank[master$value > 0] <- 2
  master$rank[master$value > 0.25] <- 3
  master$rank[master$value > 0.50] <- 4
  master$rank[master$value > 0.75] <- 5
  
  fig1 <- ggplot()+
    geom_tile(data=master, aes(x = x, y = y, fill = factor(rank)),color="#FFFFFF", size=0.2, na.rm = F)+
    scale_fill_manual(name="Percentage of swiss exports affected", values=c(gta_colour$green[1], "#ffcc00","#ef9d30","#ea5b34", gta_colour$red[1]), labels=c("0%","0%-25%","25%-50%","50%-75%",">75%"),
                      guide=guide_legend(title.position = "bottom", hjust=1))+
    scale_y_continuous(breaks=seq(1,length(cpc.names),1), labels = cpc.names, sec.axis = sec_axis(~., breaks=seq(1,length(unique(cpc.names)),1), labels = cpc.names, name = labs[2]))+
    scale_x_continuous(breaks=seq(1,length(unique(cty.names)),1), labels = cty.names)+
    labs(x=labs[1],y=labs[2])+
    gta_theme(x.bottom.angle = 45)+
    theme(panel.background = element_blank(), 
          panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
          legend.position="bottom",
          axis.text.x.bottom = element_text(hjust = 1))
  
  fig1
  
  gta_plot_saver(plot = fig1,
                 name = output.name,
                 path = output.path,
                 eps = F)
}


# DISTORTIONS 2017
heatmap(x.axis = "x.num",
        y.axis = "y.num",
        value = "distortions.2017",
        output.name = "Figure 8 - Heatmap of distortions 2017",
        labs = c("Top 10 Swiss export markets", "Top 10 Swiss CPC export sectors"))

# DISTORTIONS 2017 HIT 3 OR MORE TIMES
heatmap(x.axis = "x.num",
        y.axis = "y.num",
        value = "distortions.2017.hit3orMore",
        output.name = "Figure 9 - Heatmap of distortions 2017 hit 3 or more times",
        labs = c("Top 10 Swiss export markets", "Top 10 Swiss CPC export sectors"))

# DISTORTIONS 2017
heatmap(x.axis = "x.num",
        y.axis = "y.num",
        value = "export.incentives.2017",
        output.name = "Figure 10 - Heatmap of export incentives distortions 2017",
        labs = c("Top 10 Swiss export markets", "Top 10 Swiss CPC export sectors"))

# DISTORTIONS 2017 HIT 3 OR MORE TIMES
heatmap(x.axis = "x.num",
        y.axis = "y.num",
        value = "export.incentives.2017.hit3orMore",
        output.name = "Figure 11 - Heatmap of export incentives distortions 2017 hit 3 or more times",
        labs = c("Top 10 Swiss export markets", "Top 10 Swiss CPC export sectors"))
