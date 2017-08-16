####### 1. function EcoCast_leave_one_out_Sensitivity: function to allow us for each day in series, to create 9 EcoCast outputs, i.e. dropping out one variable in turn
# date_range (list of dates with format = date; e.g. date_range=as.character(seq(from=as.Date("2017-03-14"),to=as.Date("2017-03-24"),by="day")))
# ecocastrisk (character list of weightings in the order: Blue shark, Blue shark, Sea lions, Leatherbacks, Swordfish)
# path (pathway to EcoCast_CodeArchive folder; e.g. path = "/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive")
# envdir (pathway to sat data folder; e.g. envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep=""))
# moddir (pathway to species BRT models folder; e.g. moddir<-paste(path,"/ModRepFiles/",sep=""))
# outdir (pathway to EcoCastRuns folder; e.g. outdir <- paste(path,"/EcoCastRuns/",sep=""))
# ecocastdir (pathway to output/ folder; e.g. ecocastdir=paste(outdir,"output/",sep=""))

#Leave_on_out (LOO)

EcoCast_LOO=function(date_range,ecocastrisk,path,moddir,envdir,outdir,ecocastdir,namesrisk,sensitivitydir){
  source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/Operationalizing_code/2_load_libraries.R",chdir = TRUE)
  for(date in date_range){
    most_recent=as.character(as.Date(date)-1)
    get_date=as.Date(date)
    get_date_composite=get_date-4
    
    ############ Check to see if 4_predict_CIs.R has been run for get_date, and if not
    ############ Get a list of the paths of the env variables for get_date, or the most recent path if missing (none should be missing, this is a relic from real-time EcoCast)
    
    #1 leave out sst
    FileList_SST=list("analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_SST=unlist(lapply(FileList_SST,function(x)paste0(envdir,get_date,"/",x)))
    missingList_SST="analysed_sst.grd"
    name="SST"
    SST=list(FileList_SST,missingList_SST,name)
    
    #2 leave out sst SD
    FileList_SST_SD=list("analysed_sst.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_SST_SD=unlist(lapply(FileList_SST_SD,function(x)paste0(envdir,get_date,"/",x)))
    missingList_SST_SD="analysed_sst_sd.grd"
    name="SST_SD"
    SST_SD=list(FileList_SST_SD,missingList_SST_SD,name)
    
    #3 leave out sst and sst SD
    FileList_SST_both=list("l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_SST_both=unlist(lapply(FileList_SST_both,function(x)paste0(envdir,get_date,"/",x)))
    missingList_SST_both=c("analysed_sst.grd","analysed_sst_sd.grd")
    name="SST_both"
    SST_both=list(FileList_SST_both,missingList_SST_both,name)
    
    #4 leave out chla
    FileList_CHLA=list("analysed_sst.grd","analysed_sst_sd.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_CHLA=unlist(lapply(FileList_CHLA,function(x)paste0(envdir,get_date,"/",x)))
    missingList_CHLA="l.blendChl.grd"
    name="CHLA"
    CHLA=list(FileList_CHLA,missingList_CHLA,name)
    
    #5 leave out eke
    FileList_EKE=list("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_EKE=unlist(lapply(FileList_EKE,function(x)paste0(envdir,get_date,"/",x)))
    missingList_EKE="l.eke_mean.grd"
    name="EKE"
    EKE=list(FileList_EKE,missingList_EKE,name)
    
    #6 leave out sla
    FileList_SLA=list("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_SLA=unlist(lapply(FileList_SLA,function(x)paste0(envdir,get_date,"/",x)))
    missingList_SLA="sla.grd"
    name="SLA"
    SLA=list(FileList_SLA,missingList_SLA,name)
    
    #7 leave out sla SD
    FileList_SLA_SD=list("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_SLA_SD=unlist(lapply(FileList_SLA_SD,function(x)paste0(envdir,get_date,"/",x)))
    missingList_SLA_SD="sla_sd.grd"
    name="SLA_SD"
    SLA_SD=list(FileList_SLA_SD,missingList_SLA_SD,name)
    
    #8 leave out sla and sla SD
    FileList_SLA_both=list("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_SLA_both=unlist(lapply(FileList_SLA_both,function(x)paste0(envdir,get_date,"/",x)))
    missingList_SLA_both=c("sla.grd","sla_sd.grd")
    name="SLA_both"
    SLA_both=list(FileList_SLA_both,missingList_SLA_both,name)
    
    #9 leave out ywind
    FileList_wind=list("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd") # all of the dynamic variables, static ones will always be there
    FileList_wind=unlist(lapply(FileList_wind,function(x)paste0(envdir,get_date,"/",x)))
    missingList_wind="ywind.grd"
    name="ywind"
    wind=list(FileList_wind,missingList_wind,name)
    
    master_list=list(SST,SST_SD,SST_both,wind,SLA,SLA_SD,SLA_both,CHLA,EKE)
    
    for(i in 1:9){
      return_list=master_list[[i]]
      return_list=list("FileList_final"=master_list[[i]][[1]],"FileList_missing"=master_list[[i]][[2]],"name"=master_list[[i]][[3]])
    
    #if(!file.exists(paste0(outdir,"blshObs/predCIs/LOO/blshObs_pa_",get_date,"_",return_list$name,"_highCI.grd"))){ 
      source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/4_predict_CIs.R",chdir = TRUE)
      predCIs_master(get_date=get_date,envdir = envdir,moddir= moddir,outdir = outdir,path = path,final_path_list=return_list,sensitivitydir=sensitivitydir)
    #}
    
    ############ Now run EcoCast for get_date
    source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/5_plot_EcoCast.R",chdir = TRUE)
    Run_ecocast(preddate=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,sensitivitydir=sensitivitydir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,final_path_list=return_list)
    }
}
}

