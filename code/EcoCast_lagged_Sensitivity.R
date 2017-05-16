##### 1. function EcoCast_laged: function to allow us for each day in series, to drop out each variable over each day for a week (n=35 products)
# date_range (list of dates with format = date; e.g. date_range=as.character(seq(from=as.Date("2017-03-14"),to=as.Date("2017-03-24"),by="day")))
# ecocastrisk (character list of weightings in the order: Blue shark, Blue shark, Sea lions, Leatherbacks, Swordfish)
# path (pathway to EcoCast_CodeArchive folder; e.g. path = "/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive")
# envdir (pathway to sat data folder; e.g. envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep=""))
# moddir (pathway to species BRT models folder; e.g. moddir<-paste(path,"/ModRepFiles/",sep=""))
# outdir (pathway to EcoCastRuns folder; e.g. outdir <- paste(path,"/EcoCastRuns/",sep=""))
# ecocastdir (pathway to output/ folder; e.g. ecocastdir=paste(outdir,"output/",sep=""))

# lagged

lagged=function(variable,SEQNCE,get_date){
  master_list=list()
  for(SEQ in SEQNCE){
  if((variable=="SST")==TRUE){
    FileList_final=c("l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd")
    FileList_final=unlist(lapply(FileList_final,function(x)paste0(envdir,get_date,"/",x)))
    
    lagged_list=c("analysed_sst.grd","analysed_sst_sd.grd")
    lagged_list=unlist(lapply(lagged_list,function(x)paste0(envdir,as.character(SEQ),"/",x)))
    FileList_final=unlist(list(FileList_final,lagged_list))
    missingList=NULL
    name=paste0(variable,"_",as.character(get_date-as.Date(SEQ)))
    fulllist=list("FileList_final"=FileList_final,"missingList"=missingList,"name"=name)
    master_list[[length(master_list)+1]]=list(fulllist)
  }
    if((variable=="CHLA")==TRUE){
      FileList_final=c("analysed_sst.grd","analysed_sst_sd.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd")
      FileList_final=unlist(lapply(FileList_final,function(x)paste0(envdir,get_date,"/",x)))
      
      lagged_list=c("l.blendChl.grd")
      lagged_list=unlist(lapply(lagged_list,function(x)paste0(envdir,as.character(SEQ),"/",x)))
      FileList_final=unlist(list(FileList_final,lagged_list))
      missingList=NULL
      name=paste0(variable,"_",as.character(get_date-as.Date(SEQ)))
      fulllist=list("FileList_final"=FileList_final,"missingList"=missingList,"name"=name)
      master_list[[length(master_list)+1]]=list(fulllist)
    }
    if((variable=="EKE")==TRUE){
      FileList_final=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","sla.grd","sla_sd.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd")
      FileList_final=unlist(lapply(FileList_final,function(x)paste0(envdir,get_date,"/",x)))
      
      lagged_list=c("l.eke_mean.grd")
      lagged_list=unlist(lapply(lagged_list,function(x)paste0(envdir,as.character(SEQ),"/",x)))
      FileList_final=unlist(list(FileList_final,lagged_list))
      missingList=NULL
      name=paste0(variable,"_",as.character(get_date-as.Date(SEQ)))
      fulllist=list("FileList_final"=FileList_final,"missingList"=missingList,"name"=name)
      master_list[[length(master_list)+1]]=list(fulllist)
    }   
    if((variable=="SLA")==TRUE){
      FileList_final=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","ywind.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd")
      FileList_final=unlist(lapply(FileList_final,function(x)paste0(envdir,get_date,"/",x)))
      
      lagged_list=c("sla.grd","sla_sd.grd")
      lagged_list=unlist(lapply(lagged_list,function(x)paste0(envdir,as.character(SEQ),"/",x)))
      FileList_final=unlist(list(FileList_final,lagged_list))
      missingList=NULL
      name=paste0(variable,"_",as.character(get_date-as.Date(SEQ)))
      fulllist=list("FileList_final"=FileList_final,"missingList"=missingList,"name"=name)
      master_list[[length(master_list)+1]]=list(fulllist)
    }   
    if((variable=="YWIND")==TRUE){
      FileList_final=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","z_pt25.grd","z.grd","zsd.grd","lunillum.grd")
      FileList_final=unlist(lapply(FileList_final,function(x)paste0(envdir,get_date,"/",x)))
      
      lagged_list=c("ywind.grd")
      lagged_list=unlist(lapply(lagged_list,function(x)paste0(envdir,as.character(SEQ),"/",x)))
      FileList_final=unlist(list(FileList_final,lagged_list))
      missingList=NULL
      name=paste0(variable,"_",as.character(get_date-as.Date(SEQ)))
      fulllist=list("FileList_final"=FileList_final,"missingList"=missingList,"name"=name)
      master_list[[length(master_list)+1]]=list(fulllist)
    }   
  }
  return(master_list)
}

EcoCast_lagged=function(date_range,ecocastrisk,path,moddir,envdir,outdir,sensitivitydir,ecocastdir,namesrisk,variable){
  source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/Operationalizing_code/2_load_libraries.R",chdir = TRUE)
  for(date in date_range){
    print(date)
    most_recent=as.character(as.Date(date)-1)
    get_date=as.Date(date)
    #SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-2),as.character(get_date-3),as.character(get_date-4),as.character(get_date-5),as.character(get_date-6),as.character(get_date-7)))
    SEQNCE=unlist(list(as.character(get_date-1),as.character(get_date-7),as.character(get_date-14),as.character(get_date-21),as.character(get_date-28),as.character(get_date-30)))
    print(SEQNCE)
    get_date_composite=get_date-4
    
    ############ Get a list of the paths of the env variables for get_date, or the most recent path if missing (none should be missing, this is a relic from real-time EcoCast)
    
    master_list=lagged(variable=variable,SEQNCE = SEQNCE,get_date = get_date)
    
    for(i in 1:7){
      return_list=master_list[[i]][[1]]
      print(return_list)
    
      if(!file.exists(paste0(outdir,"blshObs/predCIs/lagged/blshObs_pa_",get_date,"_",return_list$name,"_highCI.grd"))){ 
      source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/4_predict_CIs.R",chdir = TRUE)
      predCIs_master(get_date=get_date,envdir = envdir,moddir= moddir,outdir = outdir,path = path,final_path_list=return_list,sensitivitydir=sensitivitydir)
}
    
    ############ Now run EcoCast for get_date
    source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/5_plot_EcoCast.R",chdir = TRUE)
    Run_ecocast(preddate=get_date,moddir=moddir,outdir = outdir,ecocastdir = ecocastdir,sensitivitydir = sensitivitydir,namesrisk=namesrisk,ecocastrisk=ecocastrisk,final_path_list=return_list)
    }
  }
}


