### quantify global mean and sd of 1. official output, 2. official lagged analysis (OO), 3. leave one out analyais (LOO), 4. lagged variable analyis (lagged)
###steps
# 0 create random dates
# 1 create stack for each series
# 2 mean + sd thru time
# %>% mean and st thru space (cellstats)

library(raster)
library(tidyverse)
library(reshape)
library(gridExtra)
library(grid)
library(splitstackshape)

############### run the analysis ######
# 0 create random dates
date_2012=as.character(seq(from=as.Date("2012-08-01"),to=as.Date("2012-12-27"),by="day"))
date_2015=as.character(seq(from=as.Date("2015-08-09"),to=as.Date("2015-12-31"),by="day"))
dates=unlist(list(date_2012,date_2015))
Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged" 
LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
OO_lag=c(1:8,14,30)
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")
lagged_var_names=list("SST","CHLA","EKE","ywind","SLA")
lagged_lag=c(1,7,14,21,28,30)
empty_OO=data.frame(replicate=NA,analysis=NA,mean=NA,sd=NA)
empty_OO_lagged=data.frame(replicate=NA,analysis=NA,mean=NA,sd=NA)
empty_LOO=data.frame(replicate=NA,analysis=NA,mean=NA,sd=NA)
empty_lagged_var=data.frame(replicate=NA,analysis=NA,mean=NA,sd=NA)
# analysis = OO, OO_lagged,LOO,var_lagged

removefiles=function(x){
  b=unlist(lapply(x,function(x)file.exists(x)))
  missing=grep("FALSE",b)
  if(length(missing)>0){
  x=x[-missing]
  }else{
    x=x
  }
  return(x)
 
}

for(i in 1:10){
  print(i)
  poz=sample(1:294,50,replace=F) #length(dates)=294
  OO=unlist(lapply(poz,function(x)
    paste0(OO_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[x],"_.grd")))
  a=abs(stack(removefiles(OO))*studyarea)
  b=calc(a,mean)
  OO=cellStats(b,sum)/ncell(studyarea)
  OOsd=cellStats(b,sd)
  empty_OO[i,1]=i
  empty_OO[i,2]="OO"
  empty_OO[i,3]=OO
  empty_OO[i,4]=OOsd

  for(ii in 1:length(OO_lag)){
    name=paste0("OO_lagged",OO_lag[ii])
    print(name)
    a=unlist(lapply(poz,function(x)
      paste0(OO_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",as.Date(dates[x])-OO_lag[ii],"_.grd")))
    a=abs(stack(removefiles(a))*studyarea)
    b=calc(a,mean)
    bb=cellStats(b,sum)/ncell(studyarea)
    bbsd=cellStats(b,sd)
    empty_OO_lagged[i*10+ii,1]=i
    empty_OO_lagged[i*10+ii,2]=name
    empty_OO_lagged[i*10+ii,3]=bb
    empty_OO_lagged[i*10+ii,4]=bbsd
  }
  

  for(ii in 1:length(var_names)){
    name=paste0("LOO",var_names[ii])
    print(name)
    a=unlist(lapply(poz,function(x)
      paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",as.Date(dates[x]),"_",var_names[ii],".grd")))
    a=abs(stack(removefiles(a))*studyarea)
    b=calc(a,mean)
    bb=cellStats(b,sum)/ncell(studyarea)
    bbsd=cellStats(b,sd)
    empty_LOO[i*9+ii,1]=i
    empty_LOO[i*9+ii,2]=paste0("LOO",var_names[ii])
    empty_LOO[i*9+ii,3]=bb
    empty_LOO[i*9+ii,4]=bbsd
  }
  
  for(ii in 1:length(lagged_lag)){
    for(iii in 1:length(lagged_var_names)){
      var=lagged_var_names[iii]
    name=paste0(var,"_",lagged_lag[ii])
    print(name)
    a=unlist(lapply(poz,function(x)
      paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",as.Date(dates[x]),"_",name,".grd")))
    a=abs(stack(removefiles(a))*studyarea)
    b=calc(a,mean)
    bb=cellStats(b,sum)/ncell(studyarea)
    bbsd=cellStats(b,sd)
    empty_lagged_var[i*30+(ii*6)+iii,1]=i
    empty_lagged_var[i*30+(ii*6)+iii,2]=paste0("lagged_",name)
    empty_lagged_var[i*30+(ii*6)+iii,3]=bb
    empty_lagged_var[i*30+(ii*6)+iii,4]=bbsd
    }
  }
}
  
OO=empty_OO[complete.cases(empty_OO),]
OO_lagged=empty_OO_lagged[complete.cases(empty_OO_lagged),]
#OO_lagged$lag=substring(OO_lagged$analysis, first=10, last = nchar(OO_lagged$analysis)) a
LOO=empty_LOO[complete.cases(empty_LOO),]
lagged_var=empty_lagged_var[complete.cases(empty_lagged_var),]
#lagged_var$a=substring(lagged_var$analysis, first=10, last = nchar(lagged_var$analysis))

all=rbind(OO,OO_lagged,LOO,lagged_var)
#write.csv(all,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/means_all.csv")
#########
all=read.csv("~/Desktop/EcoGit/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/means_all.csv")
all=all[,2:4]

#all$replicate=as.factor(all$replicate)
a=melt(all,id="analysis")
means=cast(a,analysis~variable,c(mean,sd))
means=means[complete.cases(means),]
means$analysis=gsub("_28","_30",means$analysis)  
means$upper=means$mean_mean+means$mean_sd
means$lower=means$mean_mean-means$mean_sd
means$test=means$analysis
b=cSplit(means, 'test', sep="_", drop=T,type.convert=FALSE)
b=as.data.frame(b)
b[36,10]=1
b[37,10]=14
b[38,10]=3
b[39,10]=2
b[40,10]=30
b[41,10]=4
b[42,10]=5
b[43,10]=6
b[44,10]=7
b[45,10]=8

means=b[,c(1,4,6,7,10)]
means$lag=means$test_3
means=means[,c(1:4,6)]
OO=means[35,]
OO_lagged=means[36:45,]
OO_lagged$analysis="OO"
OO_lagged$lag=as.numeric(OO_lagged$lag)
OO_lagged=OO_lagged[order(OO_lagged[,5]),]
OO[2,]=OO[1,]
OO[1,5]=1
OO[2,5]=30
OO$lag=as.numeric(OO$lag)

LOO=means[26:34,]
LOO[1,5]="CHLA"
LOO[2,5]="EKE"
LOO[3,5]="SLA"
LOO[4,5]="SLA_both"
LOO[5,5]="SLA_SD"
LOO[6,5]="SST"
LOO[7,5]="SST_both"
LOO[8,5]="SST_SD"
LOO[9,5]="ywind"
LOO$analysis="LOO"
LOO$variable=paste0(LOO$lag,"_LOO")
LOO$lag=1
LOO2=LOO
LOO2$lag=30
LOO=rbind(LOO,LOO2)
lagged=means[1:25,]
lagged=cSplit(lagged, 'analysis', sep="_", drop=F,type.convert=FALSE)
lagged$lag=as.numeric(lagged$lag)


s.mean=ggplot()+geom_line(data=OO_lagged, aes(lag, mean_mean, color="OO_lagged",group=analysis))+geom_ribbon(data=OO_lagged,aes(x=mean_mean,ymax=upper,ymin=lower))+
geom_line(data=lagged, aes(lag, mean_mean,color=analysis_2,group=analysis_2))+
  geom_line(data=OO, aes(lag,mean_mean,color="OO")) #+
  geom_line(data=LOO, aes(lag,mean_mean,color=variable,group=variable))
 