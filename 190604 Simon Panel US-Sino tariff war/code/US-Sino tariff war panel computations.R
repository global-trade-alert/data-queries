rm(list=ls())

library(gtalibrary)

gta_setwd()

project = '4 data queries'
query = '190604 Simon Panel US-Sino tariff war'
output.path = paste(project, query,'output/',sep='/')
data.path = paste(project, query,'data/',sep='/')
  
coverage.period = c(2009,2019)
gta.evaluation = c('Red','Amber')
intra.year.duration = T

# by target -----------------------------------------------------------------------

t.combinations = data.frame(implementers = 'United States of America', exporters ='China', 
                          nr.exporters.1 = c(1,2,0), nr.exporters.2 = c(1,999,999),label = c('Targeted','Untargeted', 'Any'))
t.combinations = rbind(t.combinations, data.frame(implementers = 'China', exporters ='United States of America', 
                                              nr.exporters.1 = c(1,2,0), nr.exporters.2 = c(1,999,999),label = c('Targeted','Untargeted', 'Any')))
t.master = data.frame()

for (i in 1:nrow(t.combinations)){
  implementers = t.combinations$implementers[i]
  exporters = t.combinations$exporters[i]
  nr.exporters = c(t.combinations$nr.exporters.1[i], t.combinations$nr.exporters.2[i])
  label = t.combinations$label[i]
  
  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     affected.flows = 'inward', 
                     implementers = implementers, 
                     keep.implementer = T, 
                     exporters = exporters,
                     keep.exporters = T,
                     nr.exporters = nr.exporters,
                     intra.year.duration = intra.year.duration

  )
  
  if (error.message == F){
  trade.coverage.estimates$importer = implementers
  trade.coverage.estimates$exporter = exporters
  trade.coverage.estimates$target.status = label
  
  t.master = rbind(t.master, trade.coverage.estimates)
  }

}

t.master$mast = 'any'
t.master = subset(t.master, select=c('importer', 'exporter', 'target.status', 'mast', names(t.master)[grep("Trade coverage", colnames(t.master))]))
names(t.master) = gsub('Trade coverage estimate for ','', names(t.master))
# by mast  ----------------------------------------------------------------


m.combinations = data.frame(implementers = 'United States of America', exporters ='China', mast = c('any','tariff','L'))
m.combinations = rbind(m.combinations, data.frame(implementers = 'China', exporters = 'United States of America', mast = c('any','tariff','L')))

mast.master = data.frame()
nr.exporters = c(0,999)

for (i in 1:nrow(m.combinations)){
  implementers = m.combinations$implementers[i]
  exporters = m.combinations$exporters[i]
  mast.chapter = m.combinations$mast[i]
  label = m.combinations$mast[i]
  if (mast.chapter == 'any'){mast.chapter = NULL}
  
  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     affected.flows = 'inward', 
                     mast.chapters = mast.chapter, 
                     keep.mast = T, 
                     implementers = implementers, 
                     keep.implementer = T, 
                     exporters = exporters,
                     keep.exporters = T,
                     nr.exporters = nr.exporters,
                     intra.year.duration = intra.year.duration

  )
  
  if (error.message == F){
    trade.coverage.estimates$importer = implementers
    trade.coverage.estimates$exporter = exporters
    trade.coverage.estimates$target.status = 'Any'
    trade.coverage.estimates$mast = label
    
    mast.master = rbind(mast.master, trade.coverage.estimates)
  }
  
}

mast.master = subset(mast.master, select=c('importer', 'exporter', 'mast', 'target.status', names(mast.master)[grep("Trade coverage", colnames(mast.master))]))
names(mast.master) = gsub('Trade coverage estimate for ','', names(mast.master))

master = rbind(mast.master, t.master)

save(master, file=paste0(data.path, 'Sino-US panel data.Rdata'))
