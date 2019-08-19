# DEFINITIONS

# CODE SETTINGS
output.path = "4 data queries/190617 Top10 Swiss export markets/output/"
data.path = "4 data queries/190617 Top10 Swiss export markets/data/"

countries <- gtalibrary::country.names
country = 756 # EXPORTING COUNTRY
range = 10 # SET RANGE FOR THE TOP VALUES
gold = c(710811, 710812, 710813, 710820) # EXCLUDE GOLD FROM SWISS EXPORTS

# IDENTIFIED CAR AND CAR RELATED HS CODES:
hs.car <- as.numeric(gtalibrary::gta_hs_code_check(codes = c(8701, 8702, 8703, 8704, 8705, 8706, 8707, 8708, 8709)))


source('0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R')
gtalibrary::gta_data_slicer(intervention.ids=trade.war.us,
                keep.interventions = T,
                gta.evaluation = c('Amber','Red'))
master.sliced=subset(master.sliced, intervention.id %in% c(63051,57917,62073))
us.affected.products=unique(as.numeric(unlist(strsplit(aggregate(affected.product~i.un,master.sliced,function(x) paste(x, collapse=', '))$affected.product, ',')))) 

