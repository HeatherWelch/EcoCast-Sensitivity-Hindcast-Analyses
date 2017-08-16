####### running grid courts to test time-series correlation ###

source("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/code/gridcorts.R")
library(raster)

####### general objects
Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged" 
LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

##### OO time-series
ecocast=list.files(OO_dir,pattern=".grd$",full.names = T)
c=grep("_2012-",ecocast,value = T)
d=grep("_2015-",ecocast,value = T)
e=unlist(list(c,d))
ecostack=stack(e)
clip_stack=ecostack*studyarea

a=unlist(lapply(e,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x)))
b=unlist(lapply(a,function(x)gsub("_.grd","",x)))
names(ecostack)=b
names(clip_stack)=b
OO_stack=clip_stack

################ LEAVE ONE OUT ANALYSIS SENSITIVITY ################
### function
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")
empty=data.frame(missing_var=NA,corellation=NA,pvalue=NA,p.GT.9.cor=NA,p.GT.05.pval=NA)
stack_function=function(variable,LOO_dir=LOO){
  a=list.files(LOO_dir,pattern = paste0(variable,".grd"),full.names = T)
  b=unlist(lapply(a,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x)))
  c=unlist(lapply(b,function(x)gsub("_.grd","",x)))
  ecostack=stack(a)
  clip_stack=ecostack*studyarea
  names(clip_stack)=c
  return(clip_stack)
}
for(i in 1:length(var_names)){
  print(var_names[i])
  new_stack=stack_function(variable=var_names[i])
  new_stack=new_stack[[1:306]]
  both=stack(OO_stack,new_stack)
  print("running gridcorts")
  r=gridcorts(both,method = "spearman",type="both")
  cor=cellStats(r[[1]],mean)
  pval=cellStats(r[[2]],mean)
  cor_values=getValues(r[[1]])
  pval_values=getValues(r[[2]])
  empty[i,1]=var_names[i]
  empty[i,2]=cor
  empty[i,3]=pval
  empty[i,4]=length(cor_values[cor_values>.9])/12936*100
  empty[i,5]=length(pval_values[pval_values<.05])/12936*100
}







##### LOO time-series
a=list.files(LOO,pattern = paste0("SST.grd"),full.names = T)
b=unlist(lapply(a,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x)))
c=unlist(lapply(b,function(x)gsub("_.grd","",x)))
ecostack=stack(a)
clip_stack=ecostack*studyarea
names(clip_stack)=c
LOO_sst_stack=clip_stack[[1:306]]

both=stack(OO_stack,LOO_sst_stack)

LOO_sst=gridcorts(both,method = "spearman",type="both")
