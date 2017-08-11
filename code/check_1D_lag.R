### check 1 day lags
################ writing out plots

library(raster)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

bJFMA1=matrix(c(-130,30,  ## define SST box
                -130,35,
                -124,35,
                -124,30,
                -130,30),
              ncol=2,byrow = T)

p=Polygon(bJFMA1)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(sps,add=T)

e=extent(sps)
rm(p,ps,sps)

############# ----------------------> Official output 12-08-02
setwd("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-02")
layers=c("analysed_sst.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","ywind.grd")
for(layer in layers){
  a=raster(paste0(getwd(),"/",layer))
  b=crop(a,e)
  name=gsub(".grd","",layer)
  name2=paste0(name,"_12_08_02")
  assign(name2,b)
}
ecoras_02=raster("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_2012-08-02_")
ecorass_02=crop(ecoras_02,e)

png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/OO_2012-08-02.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(3,3))
plot(analysed_sst_12_08_02,main="SST, 2012-08-02")
plot(l.blendChl_12_08_02,main="CHLA, 2012-08-02")
plot(l.eke_mean_12_08_02,main="EKE, 2012-08-02")
plot(sla_12_08_02,main="MSLA, 2012-08-02")
plot(ywind_12_08_02,main="ywind, 2012-08-02")
plot(ecorass_02,main="Official output, 2012-08-02")
dev.off()

############# ----------------------> LAGGED Official output 12-08-01
setwd("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-01")
layers=c("analysed_sst.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","ywind.grd")
for(layer in layers){
  a=raster(paste0(getwd(),"/",layer))
  b=crop(a,e)
  name=gsub(".grd","",layer)
  name2=paste0(name,"_12_08_01")
  assign(name2,b)
}
ecoras_01=raster("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_2012-08-01_")
ecorass_01=crop(ecoras_01,e)

png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/OO_2012-08-01.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)

par(mfrow=c(3,3))
plot(analysed_sst_12_08_01,main="SST, 2012-08-01")
plot(l.blendChl_12_08_01,main="CHLA, 2012-08-01")
plot(l.eke_mean_12_08_01,main="EKE, 2012-08-01")
plot(sla_12_08_01,main="MSLA, 2012-08-01")
plot(ywind_12_08_01,main="ywind, 2012-08-01")
plot(ecorass_01,main="Official output, 2012-08-01")
dev.off()

############# ----------------------> LAGGED variable output chla_1 day 12-08-02
ecoras_chla=raster("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/lagged/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_2012-08-02_CHLA_1")
ecorass_chla=crop(ecoras_chla,e)

# chla_07_31=raster("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-07-31/l.blendChl.grd")
# l.blendChl_2012_07_31=crop(chla_07_31,e)

png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/lagged_2012-08-01_CHLA_1.png",
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
par(mfrow=c(3,3))
plot(analysed_sst_12_08_02,main="SST, 2012-08-02")
plot(l.blendChl_12_08_01,main="CHLA, 2012-08-01")
plot(l.eke_mean_12_08_02,main="EKE, 2012-08-02")
plot(sla_12_08_02,main="MSLA, 2012-08-02")
plot(ywind_12_08_02,main="ywind, 2012-08-02")
plot(ecorass_chla,main="Lagged variable, 2012-08-02, chla 1d lag")
dev.off()

############# ----------------------> LAGGED Official output 12-07-31
# setwd("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-07-31")
# layers=c("analysed_sst.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","ywind.grd")
# for(layer in layers){
#   a=raster(paste0(getwd(),"/",layer))
#   b=crop(a,e)
#   name=gsub(".grd","",layer)
#   name2=paste0(name,"_2012-07-31")
#   assign(name2,b)
# }
# ecoras_02=raster("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_2012-07-31_")
# ecorass_02=crop(ecoras_02,e)
# 
# png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/OO_2012-07-31.png",
#     width     = 3.25,
#     height    = 3.25,
#     units     = "in",
#     res       = 1200,
#     pointsize = 4)
# 
#   par(mfrow=c(3,3))
#   plot(analysed_sst_12_08_02,main="SST, 2012-07-31")
#   plot(l.blendChl_12_08_02,main="CHLA, 2012-07-31")
#   plot(l.eke_mean_12_08_02,main="EKE, 2012-07-31")
#   plot(sla_12_08_02,main="MSLA, 2012-07-31")
#   plot(ywind_12_08_02,main="ywind, 2012-07-31")
#   plot(ecorass_02,main="Official output, 2012-07-31")
#   dev.off()
  
  
  ############# ----------------------> LAGGED Official output 12-08-02
  setwd("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-02")
  layers=c("analysed_sst.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","ywind.grd")
  for(layer in layers){
    a=raster(paste0(getwd(),"/",layer))
    b=crop(a,e)
    name=gsub(".grd","",layer)
    name2=paste0(name,"_12_08_02")
    assign(name2,b)
  }
  ecoras_02=raster("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_2012-08-02_")
  ecorass_02=crop(ecoras_02,e)
  
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/OO_2012-08-02.png",
      width     = 3.25,
      height    = 3.25,
      units     = "in",
      res       = 1200,
      pointsize = 4)
  
  par(mfrow=c(3,3))
  plot(analysed_sst_12_08_02,main="SST, 2012-08-02")
  plot(l.blendChl_12_08_02,main="CHLA, 2012-08-02")
  plot(l.eke_mean_12_08_02,main="EKE, 2012-08-02")
  plot(sla_12_08_02,main="MSLA, 2012-08-02")
  plot(ywind_12_08_02,main="ywind, 2012-08-02")
  plot(ecorass_02,main="Official output, 2012-08-02")
  dev.off()

  ################ day by day analysis
  #compare mean spatial difference for a. evnt vars, b. species layers, c. output for both OO and lagged
  
  dates=seq(as.Date("2012-08-02"),as.Date("2012-08-08"),by="day")
  Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged" 
  #LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
  OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
  var_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/"
  spp_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/"
  sensitivity_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/"
  studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
  empty=data.frame(month=NA,day=NA,year=NA,lag=NA,lagged_var=NA,analysis=NA,datatype=NA,s.mean=NA)
  
  #envt vars
  layers=c("analysed_sst.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","ywind.grd")
  for(i in 1:length(dates)){
    print(dates[i])
    lag_date=dates[i]-1
    print(lag_date)
    var_dir_date=paste0(var_dir,dates[i])
    var_dir_lag_date=paste0(var_dir,lag_date)
    chron=as.character(dates[i])
    for(ii in 1:length(layers)){
      OO=raster(paste0(var_dir_date,"/",layers[ii]))*studyarea
      OO_lag=raster(paste0(var_dir_lag_date,"/",layers[ii]))*studyarea
      q=abs(OO-OO_lag)
      r=cellStats(q,sum)/12936
      empty[5*i+ii,1]=substring(chron, first=6, last = 7)
      empty[5*i+ii,2]=substring(chron, first=9, last = 10)
      empty[5*i+ii,3]=substring(chron, first=1, last = 4)
      empty[5*i+ii,4]="One"
      empty[5*i+ii,5]=layers[ii]
      empty[5*i+ii,7]="env variable"
      empty[5*i+ii,8]=r
    }
    OO_lag=paste0(OO_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",lag_date,"_")
    OO=paste0(OO_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_")
    OO_ras_lag=raster(OO_lag)*studyarea
    OO_ras=raster(OO)*studyarea
    q=abs(OO_ras-OO_ras_lag)
    r=cellStats(q,sum)/12936
  }
  empty_var=empty[6:nrow(empty),]
  empty_var$date=paste0(empty_var$year,"-",empty_var$month,"-",empty_var$day)
  empty_var$date=as.factor(empty_var$date)
  plot(empty_var$date,empty_var$s.mean)
  empty_var2=empty_var[,c(5,8,9)]
  empty_var2$date=as.factor(empty_var2$date)
  a=melt(empty_var2,id=c("date","lagged_var"))
  #means=cast(a,lagged_var~variable,mean)
  x=dcast(a,date~lagged_var+variable)
  a=melt(x,id="date")
  a$date=as.factor(a$date)
  var=a
  ggplot(a,aes(date,value))+geom_line(aes(colour = variable,group=variable))
  
  
  
  
  
  
  
  
  
  #b. species layers
  species=c("blshObs","blshTr","casl","lbst","swor")
  empty=data.frame(month=NA,day=NA,year=NA,lag=NA,lagged_var=NA,analysis=NA,datatype=NA,s.mean=NA)
  for(i in 1:length(dates)){
    print(dates[i])
    lag_date=dates[i]-1
    print(lag_date)
    var_dir_date=paste0(var_dir,dates[i])
    var_dir_lag_date=paste0(var_dir,lag_date)
    chron=as.character(dates[i])
      OO_lag=paste0(OO_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",lag_date,"_.grd")
      print(OO_lag)
      OO=paste0(OO_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_.grd")
      print(OO)
      OO_ras_lag=raster(OO_lag)*studyarea
      OO_ras=raster(OO)*studyarea
      q=abs(OO_ras-OO_ras_lag)
      r=cellStats(q,sum)/12936
      empty[5*i+ii,1]=substring(chron, first=6, last = 7)
      empty[5*i+ii,2]=substring(chron, first=9, last = 10)
      empty[5*i+ii,3]=substring(chron, first=1, last = 4)
      empty[5*i+ii,4]="One"
      empty[5*i+ii,5]="lagged OO"
      empty[5*i+ii,7]="lagged OO"
      empty[5*i+ii,8]=r
  }
  empty$date=paste0(empty$year,"-",empty$month,"-",empty$day)
  empty_output=empty[,c(5,7,8,9)]
  empty_output=empty_output[complete.cases(empty_output),]
  empty_output$date=as.factor(empty_output$date)
  plot(empty_output$date,empty_output$s.mean)
  empty_output2=empty_output[,c(1,3,4)]
  empty_output2$date=as.factor(empty_output2$date)
  a=melt(empty_output2,id=c("date","lagged_var"))
  #means=cast(a,lagged_var~variable,mean)
  x=dcast(a,date~lagged_var+variable)
  a=melt(x,id="date")
  a$date=as.factor(a$date)
  output=a
  
  xx=rbind(output,var)
  ggplot(xx,aes(date,value))+geom_line(aes(colour = variable,group=variable))
  
  OO=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO.csv")
  OO=OO[complete.cases(OO),]
  OO$date=paste0(OO$year,"-0",OO$month,"-0",OO$day)
  OO=OO[,c(5:6,11)]
  OO=OO[OO$t.minus==1,]
  OO$variable="Lagged Official"
  OO$value=OO$s.mean
  OO=OO[,3:5]
  OO=OO[2:8,]
  
  lagged=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged.csv")
  lagged$date=paste0(lagged$year,"-0",lagged$month,"-0",lagged$day)
  lagged=lagged[lagged$lag==1,]
  lagged$variable=paste0("Lagged Variable_",lagged$missing_var)
  lagged$value=lagged$s.mean
  lagged=lagged[,12:14]
  lagged=lagged[6:40,]
  
  all=rbind(var,lagged,OO)
  ggplot(all,aes(date,value))+geom_line(aes(colour = variable,group=variable))
  
  text=ggplot()+geom_line(data=OO,aes(x=date, y=value,group=variable),color="blue")+
    geom_line(data=lagged,aes(x=date, y=value,group=variable,color=variable))+
    geom_line(data=var,aes(x=date, y=value,group=variable,color=variable),linetype=2)
  
  a=melt(OO,id="t.minus")
  OO_means=cast(a,t.minus~variable,mean)
  LOO=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/LOO.csv")
  lagged=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged.csv")
  