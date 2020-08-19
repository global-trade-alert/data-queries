rm(list = ls())

### Request:
# For each year (from 2009 on) how many interventions (of any colour) are
# - horizontal
# - non-horizontal and have no sector or HS codes
# - non-horizontal and have sector codes but no HS codes
# - non-horizontal and have HS codes but no sector codes
# - non-horizontal and have both HS codes and sector codes

library(gtalibrary)
library(tidyverse)
library(data.table)

gta_setwd()
path <- "4 data queries/200817 Interventions/"


# Load raw data
load("data/database replica/database replica - parts - base.Rdata")

# Make temporary table joining gta_intervention and sectoral as well as product data
temp=merge(select(gta_intervention, state_act_id, intervention_id, is_horizontal_measure, inception_date), 
           gta_state_act[,c("state_act_id","creation_date")], by="state_act_id")
temp$incl_hs=as.numeric(temp$intervention_id %in% gta_affected_tariff_line$intervention_id)
temp$incl_cpc=as.numeric(temp$intervention_id %in% gta_affected_sector$intervention_id)

# cleaning
temp$is_service=as.numeric(temp$intervention_id %in% subset(gta_affected_sector, sector_code>500)$intervention_id)
temp <- subset(temp, !is.na(inception_date))

temp$inception_date <- year(temp$inception_date)
temp$creation_date <- year(temp$creation_date)

# Count measures
# result <- data.frame("Type of intervention" = c("horizontal", "non-horizontal and have no sector or HS codes", "non-horizontal and have sector codes but no HS codes",
#                                                 "non-horizontal and have HS codes but no sector codes", "non-horizontal and have both HS codes and sector codes"),
#                      "Nr. of interventions (of any colour)" = c(length(unique(subset(temp, is_horizontal_measure == 1)$intervention_id)),
#                                                                length(unique(subset(temp, is_horizontal_measure == 0 & is.na(affected_products) & is.na(sector_code))$intervention_id)),
#                                                                length(unique(subset(temp, is_horizontal_measure == 0 & is.na(affected_products) & !is.na(sector_code))$intervention_id)),
#                                                                length(unique(subset(temp, is_horizontal_measure == 0 & !is.na(affected_products) & is.na(sector_code))$intervention_id)),
#                                                                length(unique(subset(temp, is_horizontal_measure == 0 & !is.na(affected_products) & !is.na(sector_code))$intervention_id))))

result <- merge(merge(merge(merge(aggregate(intervention_id ~ inception_date, subset(temp, is_horizontal_measure == 1), function(x){length(unique(x))}),
                                  aggregate(intervention_id ~ inception_date, subset(temp, is_horizontal_measure == 0 & is.na(affected_products) & is.na(sector_code)), function(x){length(unique(x))}),
                                  by = "inception_date", all = T), aggregate(intervention_id ~ inception_date, subset(temp, is_horizontal_measure == 0 & is.na(affected_products) & !is.na(sector_code)), function(x){length(unique(x))}), by = "inception_date", all = T),
                      data.frame("inception_date" = c(2009:2021), "intervention.id" = rep(0, 13)), by = "inception_date", all = T),
                aggregate(intervention_id ~ inception_date, subset(temp, is_horizontal_measure == 0 & !is.na(affected_products) & !is.na(sector_code)), function(x){length(unique(x))}), by = "inception_date", all = T)

result[is.na(result)] <- 0

names(result) <- c("Inception year", "horizontal", "non-horizontal and have no sector or HS codes", "non-horizontal and have sector codes but no HS codes",
                   "non-horizontal and have HS codes but no sector codes", "non-horizontal and have both HS codes and sector codes")

# Write table to Excel
openxlsx::write.xlsx(result, file = paste0(path, "results/GTA interventions per type.xlsx"))




