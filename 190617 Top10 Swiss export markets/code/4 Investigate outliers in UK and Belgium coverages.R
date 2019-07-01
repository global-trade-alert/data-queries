library("gtalibrary")
source("4 data queries/190617 Top10 Swiss export markets/code/0 Definitions.R")

# INVESTIGATING OUTLIERS 

# UNITED KINGDOM NEGATIVE CHANGES IN DISTORTIONS FOR CPC SECTORS: 482, 481, 449, 442, 
# BELGIUM NEGATIVE CHANGES IN DISTORTIONS FOR CPC SECTORS: 239

# Check if changes are real
gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   coverage.period = c(2010,2017),
                   exporters = country,
                   keep.exporters = T,
                   importers = "United Kingdom",
                   keep.importers = T,
                   group.importers = F,
                   cpc.sectors = 482,
                   keep.cpc = T,
                   implementer.role = "Importer")


gta_data_slicer(gta.evaluation = c("Red","Amber"),
                implementing.country = "United Kingdom",
                keep.implementer = T,
                affected.country = "Switzerland",
                revocation.period = c("2010-01-01","2100-01-01"),
                implementation.period = c("2000-01-01","2010-12-31"),
                keep.implementation.na = F,
                keep.revocation.na = T,
                keep.affected = T,
                keep.others = F,
                cpc.sectors = c(482, 481, 449, 442),
                keep.cpc = T,
                )

# RESPONSIBLE : INTERVENTION IDS 18769 and 71142

# TEST:
gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   coverage.period = c(2010,2017),
                   exporters = country,
                   keep.exporters = T,
                   importers = "United Kingdom",
                   keep.importers = T,
                   group.importers = F,
                   intervention.ids = c(18769),
                   keep.interventions = T,
                   cpc.sectors = c(482),
                   keep.cpc = T,
                   implementer.role = "Importer")

# BELGIUM

gta_data_slicer(gta.evaluation = c("Red","Amber"),
                implementing.country = "Belgium",
                keep.implementer = T,
                affected.country = "Switzerland",
                revocation.period = c("2010-01-01","2100-01-01"),
                implementation.period = c("2000-01-01","2010-12-31"),
                keep.implementation.na = F,
                keep.revocation.na = T,
                keep.affected = T,
                keep.others = F,
                cpc.sectors = c(239),
                keep.cpc = T,
)

# RESPONSIBLE : INTERVENTION IDS 14854

# TEST

gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                   coverage.period = c(2010,2017),
                   exporters = country,
                   keep.exporters = T,
                   importers = "Belgium",
                   keep.importers = T,
                   group.importers = F,
                   intervention.ids = c(14854),
                   keep.interventions = T,
                   cpc.sectors = c(239),
                   keep.cpc = T,
                   implementer.role = "Importer")
