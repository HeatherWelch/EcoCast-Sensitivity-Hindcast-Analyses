## EcoCast Sensitivity.

# Analysis 1: Lagged Official Output Sensitivity (OO)
# Analysis 2: Leave One Out Sensitivity (LOO)
# Analysis 3: Lagged variable Sensitivity (lagged)

## global objects
path = "/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
outdir <- paste("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/",sep="")
ecocastdir=paste(outdir,"output/",sep="")
ecocastrisk<-c(-0.2,-0.2,-0.05,-0.9,0.9)
namesrisk<-c("Blue shark bycatch","Blue shark tracking","Sea lions","Leatherbacks","Swordfish")

# date_range=as.character(seq(from=as.Date("2015-08-01"),to=as.Date("2015-12-31"),by="day")) ## 2015, rerun for analysis 3
date_range=as.character(seq(from=as.Date("2012-08-01"),to=as.Date("2012-12-31"),by="day"))
date_range=date_range[!date_range %in% "2012-12-28"] #missing chla from this day
#date_range=as.character(seq(from=as.Date("2012-08-01"),to=as.Date("2012-12-31"),by="day"))
#date_range=as.character(seq(from=as.Date("2012-12-31"),to=as.Date("2012-12-31"),by="day")) ## restarting loo again
#date_range=as.character(seq(from=as.Date("2012-12-29"),to=as.Date("2012-12-31"),by="day"))

##################### # Analysis 1: Lagged Official Output Sensitivity (OO)

## 1. define EcoCast_hindcast() arguments
sensitivitydir="OO"
source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/EcoCast_hindcast.R",chdir = TRUE)

EcoCast_hindcast(date_range=date_range,ecocastrisk=ecocastrisk,path=path,moddir=moddir,envdir=envdir,outdir=outdir,ecocastdir=ecocastdir,namesrisk=namesrisk,sensitivitydir=sensitivitydir)


##################### # Analysis 2: Leave One Out Sensitivity (LOO)
sensitivitydir="LOO"
source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/EcoCast_leave_one_out_Sensitivity.R",chdir = TRUE)

EcoCast_LOO(date_range=date_range,ecocastrisk=ecocastrisk,path=path,moddir=moddir,envdir=envdir,outdir=outdir,ecocastdir=ecocastdir,namesrisk=namesrisk,sensitivitydir=sensitivitydir)


##################### # Analysis 3: Lagged Sensitivity (LOO)
sensitivitydir="lagged"
#var_list=list("SST","CHLA","EKE","YWIND","SLA")
#var_list=list("SST")
#var_list=list("CHLA","EKE","YWIND","SLA")
#var_list=list("CHLA")
#var_list=list("EKE","YWIND","SLA")
#var_list=list("EKE")
var_list=list("YWIND","SLA")

source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/EcoCast_lagged_Sensitivity.R",chdir = TRUE)

lapply(var_list,FUN=EcoCast_lagged,date_range=date_range,ecocastrisk=ecocastrisk,path=path,moddir=moddir,envdir=envdir,outdir=outdir,ecocastdir=ecocastdir,sensitivitydir=sensitivitydir,namesrisk=namesrisk)
