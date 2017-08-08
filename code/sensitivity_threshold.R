### thresholding sensitivity
# natural elbows were not apparent in the 4 difference metrics
# trying a threshold: when mean value is > mean ecocast value +/- SD

###1. find mean ecocast value
###2. find spatial SD ecocast value
###3. find mean value for each sensitivity layer
###4. plot w threshold line

library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)

OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

###1. find mean ecocast value, ####2. find spatial SD ecocast value
ecocast=list.files(OO_dir,pattern=".grd$",full.names = T)
c=grep("_2012-",ecocast,value = T)
d=grep("_2015-",ecocast,value = T)
e=unlist(list(c,d))
ecostack=stack(e)
clip_stack=ecostack*studyarea
sd=calc(clip_stack, sd)
sd_per_pixel=cellStats(sd,mean)
means=calc(clip_stack, mean)
r=cellStats(means,mean) # spatial sum r=0.4433651
s=cellStats(means,sd) # spatial standard deviation s=0.2055143
#r+s=0.6488794
#r-s=0.2378508

## trying absolute value
clip_stack_abs=abs(clip_stack)
means=calc(clip_stack_abs, mean)
r_abs=cellStats(means,mean) # spatial sum r=0.5333812
s_abs=cellStats(means,sd) # spatial standard deviation s=0.1612443
upper=r_abs+s_abs
lower=r_abs-s_abs
#r+s=0.6946254
#r-s=0.3721369

###3. find mean value for each sensitivity layer

################ LEAVE ONE OUT ANALYSIS SENSITIVITY ################
LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")

### not absolute value
empty=data.frame(variable=NA,s.mean=NA,difference_from_threshold=NA,acceptible=NA)
stack_function=function(variable,LOO_dir=LOO){
  a=list.files(LOO_dir,pattern = paste0(variable,".grd"),full.names = T)
  b=unlist(lapply(a,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x)))
  c=unlist(lapply(b,function(x)gsub("_.grd","",x)))
  ecostack=stack(a)
  clip_stack=ecostack*studyarea
  names(clip_stack)=c
  means=calc(clip_stack, mean)
  r=cellStats(means,mean) # spatial sum 
  s=cellStats(means,sd) # spatial standard deviation 
  return(r)
}
for(i in 1:length(var_names)){
  print(var_names[i])
  difference=stack_function(variable=var_names[i])
  if(difference<0.2378508){
  delta=0.2378508-difference
  }
  if(difference>0.6488794){
    delta=difference-0.6488794
  }
  if(difference<0.6488794 & difference>0.2378508){
    delta="within threshold"
  }
  empty[i,1]=var_names[i]
  empty[i,2]=difference
  empty[i,3]=delta
  if(difference<0.6488794 & difference>0.2378508){
  empty[i,4]="yes"
  }else{
    empty[i,4]="no"
  }
}

### absolute value
empty_abs=data.frame(variable=NA,s.mean=NA,difference_from_threshold=NA,acceptible=NA)
stack_function_abs=function(variable,LOO_dir=LOO){
  a=list.files(LOO_dir,pattern = paste0(variable,".grd"),full.names = T)
  b=unlist(lapply(a,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x)))
  c=unlist(lapply(b,function(x)gsub("_.grd","",x)))
  ecostack=stack(a)
  clip_stack=ecostack*studyarea
  clip_stack_abs=abs(clip_stack)
  names(clip_stack_abs)=c
  means=calc(clip_stack_abs, mean)
  r=cellStats(means,mean) # spatial sum 
  s=cellStats(means,sd) # spatial standard deviation 
  return(r)
}
for(i in 1:length(var_names)){
  print(var_names[i])
  difference=stack_function_abs(variable=var_names[i])
  if(difference<lower){
    delta=lower-difference
  }
  if(difference>upper){
    delta=difference-upper
  }
  if(difference<upper & difference>lower){
    delta="within threshold"
  }
  empty_abs[i,1]=var_names[i]
  empty_abs[i,2]=difference
  empty_abs[i,3]=delta
  if(difference<upper & difference>lower){
    empty_abs[i,4]="yes"
  }else{
    empty_abs[i,4]="no"
  }
}


###### new plan, correlations (abandon, can be correlated while still having a large difference)


