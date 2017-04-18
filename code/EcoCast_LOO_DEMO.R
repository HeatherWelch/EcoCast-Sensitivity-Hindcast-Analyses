## sample script demoing how to run: 1. function EcoCast_LOO: function to allow us for each day in series, to create 9 EcoCast outputs, i.e. dropping out one variable in turn

## 1. define EcoCast_hindcast() arguments

path = "/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
outdir <- paste("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Trial/EcoCastRuns/",sep="")
ecocastdir=paste(outdir,"output/",sep="")

#########example with only one set of species weightings
ecocastrisk<-c(-0.2,-0.2,-0.05,-0.9,0.9)

date_range=as.character(seq(from=as.Date("2017-03-17"),to=as.Date("2017-03-19"),by="day"))
namesrisk<-c("Blue shark bycatch","Blue shark tracking","Sea lions","Leatherbacks","Swordfish")

source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/EcoCast_leave_one_out_Sensitivity.R",chdir = TRUE)

EcoCast_LOO(date_range=date_range,ecocastrisk=ecocastrisk,path=path,moddir=moddir,envdir=envdir,outdir=outdir,ecocastdir=ecocastdir,namesrisk=namesrisk)
