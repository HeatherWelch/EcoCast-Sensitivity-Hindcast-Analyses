##### 1. function EcoCast_hindcast: function to allow us to batch run EcoCast for various date ranges and weighting combinations
# date_range (list of dates with format = date; e.g. date_range=as.character(seq(from=as.Date("2017-03-14"),to=as.Date("2017-03-24"),by="day")))
# ecocastrisk (character list of weightings in the order: Blue shark, Blue shark, Sea lions, Leatherbacks, Swordfish)
# path (pathway to EcoCast_CodeArchive folder; e.g. path = "/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive")
# envdir (pathway to sat data folder; e.g. envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep=""))
# moddir (pathway to species BRT models folder; e.g. moddir<-paste(path,"/ModRepFiles/",sep=""))
# outdir (pathway to EcoCastRuns folder; e.g. outdir <- paste(path,"/EcoCastRuns/",sep=""))
# ecocastdir (pathway to output/ folder; e.g. ecocastdir=paste(outdir,"output/",sep=""))

EcoCast_hindcast=function(date_range,ecocastrisk,path,moddir,envdir,outdir,ecocastdir,namesrisk,sensitivitydir){
  source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/Operationalizing_code/2_load_libraries.R",chdir = TRUE)
  for(date in date_range){
    most_recent=as.character(as.Date(date)-1)
    get_date=as.Date(date)
    get_date_composite=get_date-4
    
    ############ Check to see if 4_predict_CIs.R has been run for get_date, and if not
    ############ Get a list of the paths of the env variables for get_date, or the most recent path if missing (none should be missing, this is a relic from real-time EcoCast)
    FileList_get_date=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$") # all the files from get_date
    FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
    FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
    FileList_final=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$",full.names = TRUE) # start of final list to pass to preCIs script
    return_list=list("FileList_final"=FileList_final,"FileList_missing"=FileList_missing)
    
    if(!file.exists(paste0(outdir,"blshObs/predCIs/OO/blshObs_pa_",get_date,"__highCI.grd"))){ 
      source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/4_predict_CIs.R",chdir = TRUE)
      predCIs_master(get_date=get_date,envdir = envdir,moddir= moddir,outdir = outdir,path = path,final_path_list=return_list,sensitivitydir=sensitivitydir)
    }
    
    ############ Now run EcoCast for get_date
    source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/5_plot_EcoCast.R",chdir = TRUE)
    Run_ecocast(preddate=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,sensitivitydir=sensitivitydir,final_path_list=return_list)
  }
}
