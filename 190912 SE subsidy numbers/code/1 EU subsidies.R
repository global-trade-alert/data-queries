library(gtalibrary)
library(xlsx)
library(tidyverse)

gta_setwd()

# DEFINITIONS
output.path = "4 data queries/190912 SE subsidy numbers/output/"
mast.chapter = "L" # SUBSIDIES
manufacturing <- gtalibrary::hs.codes$hs.code[hs.codes$is.agriculture==F & hs.codes$is.raw.material==F & ! hs.codes$hs.code %in% c(seq(410000,419999,1))]
eu28 <- gtalibrary::country.correspondence$un_code[country.correspondence$name=="EU-28"]

# QUERY FROM SIMON: UPDATE [[NUMBERS]] IN THE REPORT


# 1
# ...
# If the agricultural sector is omitted and the focus is on manufactured goods, 
# then a total of [[22%]] of world exports faced such subsidised rivals.
# ...

gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   hs.codes = manufacturing,
                   keep.hs = T,
                   mast.chapters = mast.chapter,
                   keep.mast = T,
                   implementer.role = "Importer",
                   affected.flows = "Inward",
                   coverage.period = c(2019,2019),
                   trade.data = 2017)

worldASubsidies = trade.coverage.estimates[1,ncol(trade.coverage.estimates)]
worldASubsidies


# 2
# ...
# As of September, [[18.5%]] of extra-EU manufacturing exports competed 
# against subsidised local rivals in foreign markets. It would seem there 
# is plenty of subsidisation abroad that might concern European policymakers.
# ...

gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   exporters = eu28,
                   keep.exporters = F,
                   importers = eu28,
                   keep.importers = F,
                   hs.codes = manufacturing,
                   keep.hs = T,
                   mast.chapters = mast.chapter,
                   keep.mast = T,
                   implementer.role = "Importer",
                   coverage.period = c(2019,2019),
                   trade.data = 2017)

extraEUSubsidies = trade.coverage.estimates[1,ncol(trade.coverage.estimates)]
extraEUSubsidies


# 3
# ...
# Since November 2008 a total of [[1,135]] subsidies have been awarded by 
# European public bodies, [[898]] of them to the manufacturing sector. 
# ...

gta_data_slicer(gta.evaluation = c("Red","Amber"),
                implementing.country = eu28,
                keep.implementer = T,
                mast.chapters = mast.chapter,
                keep.mast = T,
                implementation.period = c(NA,paste(as.Date(Sys.Date()))),
                keep.implementation.na = F)

# Total EU subsides
totEuSubs <- aggregate(intervention.id ~ year(date.implemented), master.sliced, function(x) length(unique(x)))
totEuSubs <- sum(totEuSubs$intervention.id)
totEuSubs

# Total EU Manufacturing subsidies
totEuSubsManu <- cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=",")
totEuSubsManu <- aggregate(intervention.id ~ year(date.implemented), subset(totEuSubsManu, affected.product %in% manufacturing), function(x) length(unique(x)))
totEuSubsManu <- sum(totEuSubsManu$intervention.id)
totEuSubsManu


# 4
# ...
# Indeed, so pervasive has been the build-up of EU subsidies over the past 
# decade that, as of September 2019, [[38.2%]] of imports from outside of the 
# European Union compete against local firms that have received some type 
# of financial support from an EU public institution
# ...

# TAKEN FROM GTA24 REPORT: 10 - Sectoral subsidy accord.R, Line 375
gta_trade_coverage(gta.evaluation = c("red","amber"),
                   affected.flows = "inward",
                   coverage.period = c(2019,2019),
                   importers = eu28,
                   keep.importers = F,
                   exporters = eu28,
                   keep.exporters = T,
                   hs.codes = manufacturing,
                   keep.hs = T,
                   trade.data = 2017)

EuSubsidies = trade.coverage.estimates[1,ncol(trade.coverage.estimates)]
EuSubsidies

# WRITE EXCEL
results <- data.frame(Var = c("Number23", "Number24", "Number25.1", "Number25.2", "Number29"),
                      Value = c(worldASubsidies, extraEUSubsidies, totEuSubs, totEuSubsManu, EuSubsidies))

write.xlsx(x = results, file=paste0(output.path, "New numbers for paper.xlsx"), row.names=F, append = F)
