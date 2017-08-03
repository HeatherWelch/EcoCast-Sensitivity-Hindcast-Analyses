################ LAGGED VARIABLE SENSITIVITY ################

### code to quantify ecocast sensitivity to missing data
## # pixels in SA = 12936 ncell(studyarea)

library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(reshape2)

####### general objects
Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged"
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")

######## set up data
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
empty=data.frame(month=NA,day=NA,year=NA,missing_var=NA,s.mean=NA,s.SD=NA,p.GT.5=NA,p.GT.25=NA,p.GT.1=NA,lag=NA)

######## run quantification
# i=date
# ii=variable for date
# iii=lag for variable for date

var_names=unlist(list("SST","CHLA","EKE","YWIND","SLA"))
lags=c(1,7,14,21,28,30)
for(i in 1:length(b)){ #missing 152, and start of 2015 (missing 12/29, 12/30) #153:length(b), 172:length(b)
  print(b[i])
  OO=clip_stack[[i]]
  for(iii in 1:length(lags)){
    print(paste0("lag is ",lags[iii]))
  SST=paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SST_",lags[iii],".grd")
  CHLA=paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_CHLA_",lags[iii],".grd")
  EKE=paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_EKE_",lags[iii],".grd")
  SLA=paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SLA_",lags[iii],".grd")
  YWIND=paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_YWIND_",lags[iii],".grd")
  vars=unlist(list(SST,CHLA,EKE,YWIND,SLA))
  var_list=stack(vars)
  for(ii in 1:length(vars)){
    print(vars[ii])
    q=abs(OO-var_list[[ii]])
    r=cellStats(q,sum) # spatial sum
    s=cellStats(q,sd) # spatial standard deviation
    t=cellStats(q>.5,sum)/12936 # % cells where difference > .5
    u=cellStats(q>.25,sum)/12936 # % cells where difference > .25
    v=cellStats(q>.1,sum)/12936 # % cells where difference > .1
    empty[30*i+(ii*6)+iii,1]=substring(names(clip_stack[[i]]), first=7, last = 8)
    empty[30*i+(ii*6)+iii,2]=substring(names(clip_stack[[i]]), first=10, last = 11)
    empty[30*i+(ii*6)+iii,3]=substring(names(clip_stack[[i]]), first=2, last = 5)
    empty[30*i+(ii*65)+iii,4]=var_names[ii]
    empty[30*i+(ii*6)+iii,5]=r
    empty[30*i+(ii*6)+iii,6]=s
    empty[30*i+(ii*6)+iii,7]=t
    empty[30*i+(ii*6)+iii,8]=u
    empty[30*i+(ii*6)+iii,9]=v
    empty[30*i+(ii*6)+iii,10]=lags[iii]
  }
  }
}

######## write out csv
DF_complete=empty[complete.cases(empty),]
write.csv(DF_complete,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged.csv")

####### make some plots, averaged across month
DF_complete=DF_complete[,c(4:10)]
a=melt(DF_complete,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="s.mean")
means=cast(a,missing_var~lag,mean,subset=variable=="s.SD")
# means2=dcast(a,missing_var~lag,value.var = "p.GT.1")
# means2=reshape(DF_complete,direction = "wide",idvar="lag",timevar="missing_var")
means_s.mean=a[a$variable=="s.mean",]
means2=reshape(DF_complete,direction = "wide",idvar="lag",timevar="missing_var")
means=cast(means_s.mean,missing_var~variable,mean)

##make %
means$p.GT.1=means$p.GT.1*100
means$p.GT.25=means$p.GT.25*100
means$p.GT.5=means$p.GT.5*100

## normalizing between 1:0
# range01 <- function(x){(x-min(x))/(max(x)-min(x))} 
# means01=means
# means01=as.data.frame(lapply(means01,FUN=range01))
# means01$t.minus=means$t.minus

