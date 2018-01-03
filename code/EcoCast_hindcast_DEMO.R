## sample script demoing how to run: 1. function EcoCast_hindcast: function to allow us to batch run EcoCast for various date ranges and weighting combinations

## 1. define EcoCast_hindcast() arguments

path = "/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive"
moddir<-paste(path,"/ModRepFiles/",sep="")
envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
outdir <- paste("/Volumes/EcoCast_SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/",sep="")
ecocastdir=paste(outdir,"output/",sep="")
sensitivitydir="OO"

source("/Volumes/SeaGate/Operationalization_Sensitivity/EcoCast-Sensitivity-Hindcast-Analyses/code/EcoCast_hindcast.R",chdir = TRUE)

#########example with only one set of species weightings
ecocastrisk<-c(-0.1,-0.1,-0.05,-0.9,0.9)

date_range=as.character(seq(from=as.Date("2017-03-14"),to=as.Date("2017-03-16"),by="day"))
namesrisk<-c("Blue shark bycatch","Blue shark tracking","Sea lions","Leatherbacks","Swordfish")

EcoCast_hindcast(date_range=date_range,ecocastrisk=ecocastrisk,path=path,moddir=moddir,envdir=envdir,outdir=outdir,ecocastdir=ecocastdir,namesrisk=namesrisk,sensitivitydir=sensitivitydir)


#########example with multiple sets of species weightings
ecocastrisk1<-c(-0.4,-0.2,-0.05,-0.9,0.9)
ecocastrisk2<-c(-0.5,-0.2,-0.05,-0.9,0.9)
ecocastrisk3<-c(-0.6,-0.2,-0.05,-0.9,0.9)

ecocastrisk_L=list(ecocastrisk1,ecocastrisk2,ecocastrisk3)

date_range=as.character(seq(from=as.Date("2017-03-14"),to=as.Date("2017-03-16"),by="day"))
namesrisk<-c("Blue shark bycatch","Blue shark tracking","Sea lions","Leatherbacks","Swordfish")

lapply(ecocastrisk_L,FUN=EcoCast_hindcast,date_range=date_range,path=path,moddir=moddir,envdir=envdir,outdir=outdir,ecocastdir=ecocastdir,namesrisk=namesrisk,sensitivitydir=sensitivitydir)
