fin=subset(cSplit(master, which(colnames(master)=="affected.sector"), direction="long", sep=","), 
           affected.sector>710 &
          affected.sector<720)

master$fin=as.numeric(master$intervention.id %in% fin$intervention.id)

acts=aggregate(intervention.id ~ fin + mast.chapter, subset(master, is.na(date.implemented)==F & gta.evaluation!="Green"), function(x) length(unique(x)))
acts=reshape(acts, idvar=c("mast.chapter"),timevar="fin", direction="wide")
acts[is.na(acts)]=0

colnames(acts)=c("MAST chapter", "Harmful interventions ever implemented affecting non-financial sectors", "Harmful interventions ever implemented affecting the financial sector")
write.xlsx(acts, file="financial sector.xlsx", row.names=F)
