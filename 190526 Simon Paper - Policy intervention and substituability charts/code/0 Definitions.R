# DEFINITIONS

# PATHS
output.path <- "4 data queries/190526 Simon Paper - Policy intervention and substituability charts/output/"
data.path <- "4 data queries/190526 Simon Paper - Policy intervention and substituability charts/data/"
resources.path <- "4 data queries/190526 Simon Paper - Policy intervention and substituability charts/resources/"

# LOAD GTA COLOURS
gtalibrary::gta_colour_palette()

# DATE DEFINITIONS
# November 2008 - December 2016
period.1 <- c("2008-11-01", "2016-12-31")
# November 2008 - December 2010
period.2 <- c("2008-11-01", "2010-12-31")
# January 2011 - December 2016
period.3 <- c("2011-01-01", "2016-12-31")
periods <- list(period.1, period.2, period.3)


# COUNTRY GROUPS
correspondence <- gtalibrary::country.correspondence
country.names <- gtalibrary::country.names

# All
# - All nations for which data is available (denoted "All")
all <- country.names[,c("un_code","name")]

# G20
# - G20
g20 <- country.names$un_code[country.names$is.g20==T]

# nextG20 
# - "Next 20" (being the 20 nations whose total exports and imports in 2007 where the largest that are not members of the G20)
gta_trade_value_bilateral(trade.data = 2007)
nextg20 <- aggregate(trade.value ~ a.un, trade.base.bilateral, function(x) sum(x)) # ONLY USING EXPORTS TO CALCULATE LARGEST, SHOULD IMPORTS BE INCLUDED TOO?
nextg20 <- subset(nextg20, ! a.un %in% g20)
nextg20 <- nextg20[with(nextg20, order(-trade.value)),]
row.names(nextg20) <- NULL
nextg20 <- nextg20[1:20,c("a.un")]

groups <- list(all$un_code, g20, nextg20)
groups.name <- c("all","g20","nextg20")


# REMOVE UNUSED SETS
rm(correspondence, parameter.choice.trade.base, trade.base.bilateral)