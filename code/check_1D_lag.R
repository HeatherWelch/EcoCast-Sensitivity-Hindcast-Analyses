### check 1 day lags
################ writing out plots

library(raster)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(rasterVis)

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
  
  ###############################################################################################
  ############################ check 1d lay 08.15.2017 CHLA
  dates=seq(as.Date("2012-08-01"),as.Date("2012-08-15"),by = "day")
  studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
  
  bJFMA1=matrix(c(-140,25,  ## define SST box
                  -140,50,
                  -110,50,
                  -110,25,
                  -140,25),
                ncol=2,byrow = T)
  p=Polygon(bJFMA1)
  ps=Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  plot(sps,add=T)
  
  e=extent(sps)
  rm(p,ps,sps)
  
  
  ### read in chla
  lss=list()
for(i in 1:length(dates)){
  print(i)
  date=as.character(dates[i])
  print(date)
    ev_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/"
    date_dir=paste0(ev_dir,date,"/l.blendChl.grd")
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("chl_",date)
    assign(name,b)
}
  ### read in lbst lagged
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Lagged_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/Lagged"
    date_dir=paste0(paste0(Lagged_dir_lbst,"/lbst_pa_",dates[i],"_CHLA_1_mean.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("lbst_CHLA_1dlag",date)
    assign(name,b)
  }
  
  ### read in lbst OO
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    OO_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/OO"
    date_dir=paste0(paste0(OO_dir_lbst,"/lbst_pa_",dates[i],"__mean.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("lbst_OO_",date)
    assign(name,b)
  }
  
  ### read in ecocast lagged
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged"
    date_dir=paste0(paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_CHLA_1.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EcoCast_Lagged_CHL_1_",date)
    assign(name,b)
  }
  
  ### read in ecocast OO
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Ecocast_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO"
    date_dir=paste0(paste0(Ecocast_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EcoCast_OO_",date)
    assign(name,b)
  }
  
  
  myTheme_CHLA=rasterTheme(region=brewer.pal('RdBu', n=11))
  myTheme_lbst=rasterTheme(region=brewer.pal('PiYG', n=11))
  myTheme_EcoLagged=rasterTheme(region=brewer.pal('PRGn', n=11))
  #myTheme_Eco_OO=rasterTheme(region=brewer.pal('RdYlBu', n=11))
  
  #matrix 1: rows="2012-08-01" "2012-08-02" "2012-08-03", cols= CHLA, lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/chl_08.01-08.03.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`chl_2012-08-01`,main="CHLA, 2012-08-01",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_CHLA_1dlag2012-08-01`,main="Lbst chl1D, 2012-08-01",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-08-01`,main="Lbst_OO, 2012-08-01",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_CHL_1_2012-08-01`,main="EcoLag chl1D, 2012-08-01",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-08-01`,main="Eco_OO, 2012-08-01",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`chl_2012-08-02`,main="CHLA, 2012-08-02",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_CHLA_1dlag2012-08-02`,main="Lbst chl1D, 2012-08-02",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-08-02`,main="Lbst_OO, 2012-08-02",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_CHL_1_2012-08-02`,main="EcoLag chl1D, 2012-08-02",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-08-02`,main="Eco_OO, 2012-08-02",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`chl_2012-08-03`,main="CHLA, 2012-08-03",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_CHLA_1dlag2012-08-03`,main="Lbst chl1D, 2012-08-03",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-08-03`,main="Lbst_OO, 2012-08-03",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_CHL_1_2012-08-03`,main="EcoLag chl1D, 2012-08-03",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-08-03`,main="Eco_OO, 2012-08-03",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  #matrix 2: rows="2012-08-04" "2012-08-05" "2012-08-06", cols= CHLA, lbstLagged,lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/chl_08.04-08.06.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`chl_2012-08-04`,main="CHLA, 2012-08-04",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_CHLA_1dlag2012-08-04`,main="Lbst chl1D, 2012-08-04",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-08-04`,main="Lbst_OO, 2012-08-04",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_CHL_1_2012-08-04`,main="EcoLag chl1D, 2012-08-04",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-08-04`,main="Eco_OO, 2012-08-04",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`chl_2012-08-05`,main="CHLA, 2012-08-05",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_CHLA_1dlag2012-08-05`,main="Lbst chl1D, 2012-08-05",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-08-05`,main="Lbst_OO, 2012-08-05",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_CHL_1_2012-08-05`,main="EcoLag chl1D, 2012-08-05",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-08-05`,main="Eco_OO, 2012-08-05",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`chl_2012-08-06`,main="CHLA, 2012-08-06",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_CHLA_1dlag2012-08-06`,main="Lbst chl1D, 2012-08-06",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-08-06`,main="Lbst_OO, 2012-08-06",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_CHL_1_2012-08-06`,main="EcoLag chl1D, 2012-08-06",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-08-06`,main="Eco_OO, 2012-08-06",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  #matrix 3: rows="2012-08-07" "2012-08-08" "2012-08-09", cols= CHLA, lbstLagged,lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/chl_08.07-08.09.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`chl_2012-08-07`,main="CHLA, 2012-08-07",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_CHLA_1dlag2012-08-07`,main="Lbst chl1D, 2012-08-07",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-08-07`,main="Lbst_OO, 2012-08-07",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_CHL_1_2012-08-07`,main="EcoLag chl1D, 2012-08-07",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-08-07`,main="Eco_OO, 2012-08-07",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`chl_2012-08-08`,main="CHLA, 2012-08-08",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_CHLA_1dlag2012-08-08`,main="Lbst chl1D, 2012-08-08",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-08-08`,main="Lbst_OO, 2012-08-08",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_CHL_1_2012-08-08`,main="EcoLag chl1D, 2012-08-08",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-08-08`,main="Eco_OO, 2012-08-08",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`chl_2012-08-09`,main="CHLA, 2012-08-09",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_CHLA_1dlag2012-08-09`,main="Lbst chl1D, 2012-08-09",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-08-09`,main="Lbst_OO, 2012-08-09",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_CHL_1_2012-08-09`,main="EcoLag chl1D, 2012-08-09",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-08-09`,main="Eco_OO, 2012-08-09",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  #matrix 4: rows="2012-08-10","2012-08-11","2012-08-12" , cols= CHLA, lbstLagged,lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/chl_08.10-08.12.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`chl_2012-08-10`,main="CHLA, 2012-08-10",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_CHLA_1dlag2012-08-10`,main="Lbst chl1D, 2012-08-10",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-08-10`,main="Lbst_OO, 2012-08-10",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_CHL_1_2012-08-10`,main="EcoLag chl1D, 2012-08-10",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-08-10`,main="Eco_OO, 2012-08-10",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`chl_2012-08-11`,main="CHLA, 2012-08-11",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_CHLA_1dlag2012-08-11`,main="Lbst chl1D, 2012-08-11",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-08-11`,main="Lbst_OO, 2012-08-11",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_CHL_1_2012-08-11`,main="EcoLag chl1D, 2012-08-11",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-08-11`,main="Eco_OO, 2012-08-11",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`chl_2012-08-12`,main="CHLA, 2012-08-12",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_CHLA_1dlag2012-08-12`,main="Lbst chl1D, 2012-08-12",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-08-12`,main="Lbst_OO, 2012-08-12",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_CHL_1_2012-08-12`,main="EcoLag chl1D, 2012-08-12",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-08-12`,main="Eco_OO, 2012-08-12",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  #matrix 5: rows="2012-08-13","2012-08-14","2012-08-15" , cols= CHLA, lbstLagged,lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/chl_08.13-08.15.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`chl_2012-08-13`,main="CHLA, 2012-08-13",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_CHLA_1dlag2012-08-13`,main="Lbst chl1D, 2012-08-13",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-08-13`,main="Lbst_OO, 2012-08-13",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_CHL_1_2012-08-13`,main="EcoLag chl1D, 2012-08-13",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-08-13`,main="Eco_OO, 2012-08-13",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`chl_2012-08-14`,main="CHLA, 2012-08-14",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_CHLA_1dlag2012-08-14`,main="Lbst chl1D, 2012-08-14",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-08-14`,main="Lbst_OO, 2012-08-14",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_CHL_1_2012-08-14`,main="EcoLag chl1D, 2012-08-14",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-08-14`,main="Eco_OO, 2012-08-14",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`chl_2012-08-15`,main="CHLA, 2012-08-15",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_CHLA_1dlag2012-08-15`,main="Lbst chl1D, 2012-08-15",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-08-15`,main="Lbst_OO, 2012-08-15",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_CHL_1_2012-08-15`,main="EcoLag chl1D, 2012-08-15",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-08-15`,main="Eco_OO, 2012-08-15",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  ## calculating difference, mean
  empty=data.frame(time=NA,lbst_lagged_diff=NA,lbst_OO_diff=NA,eco_lagged_diff=NA,eco_OO_diff=NA)
  for(i in 2:length(dates)){
    print(paste0("today is ",dates[i]))
    lbst_lag=get(paste0("lbst_CHLA_1dlag",dates[i]));print(paste0("lbst_CHLA_1dlag",dates[i]))
    lbst_OO=get(paste0("lbst_OO_",dates[i]));print(paste0("lbst_OO_",dates[i]))
    lbst_OOm1=get(paste0("lbst_OO_",dates[i]-1));print(paste0("lbst_OO_",dates[i]-1))
    eco_lag=get(paste0("EcoCast_Lagged_CHL_1_",dates[i]));print(paste0("EcoCast_Lagged_CHL_1_",dates[i]))
    eco_OO=get(paste0("EcoCast_OO_",dates[i]))
    eco_OOm1=get(paste0("EcoCast_OO_",dates[i]-1))
    lbst_lag=abs(lbst_OO-lbst_lag)
    lbst_OOd=abs(lbst_OO-lbst_OOm1)
    eco_lag=abs(eco_OO-eco_lag)
    eco_OOd=abs(eco_OO-eco_OOm1)
    q=cellStats(lbst_lag,mean)
    r=cellStats(lbst_OOd,mean)
    s=cellStats(eco_lag,mean)
    t=cellStats(eco_OOd,mean)
    empty[i,1]=as.character(dates[i])
    empty[i,2]=q
    empty[i,3]=r
    empty[i,4]=s
    empty[i,5]=t
  }
  
  a=melt(empty,id.vars = "time")
  a=a[complete.cases(a),]
  a$time=as.factor(a$time)
  
  master=ggplot()+geom_line(data=a,aes(x=time,y=value,color=variable,group=variable))
  ms2=master+labs(y="Mean of per pixel difference from zero lag")+ggtitle("Comparison of lagged and OO analyses for first two weeks of Aug 2012")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ 
    theme(text = element_text(size=15))+ theme(legend.position="bottom",legend.key = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))+
    theme(plot.title = element_text(hjust = 0.5)) 
  
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/daily_means_m.png",width=1400,height=600,units='px',pointsize=100)
  ms2
  dev.off()
  
  ## calculating difference, sum/#pixels(11424)
  empty=data.frame(time=NA,lbst_lagged_diff=NA,lbst_OO_diff=NA,eco_lagged_diff=NA,eco_OO_diff=NA)
  for(i in 2:length(dates)){
    print(paste0("today is ",dates[i]))
    lbst_lag=get(paste0("lbst_CHLA_1dlag",dates[i]));print(paste0("lbst_CHLA_1dlag",dates[i]))
    lbst_OO=get(paste0("lbst_OO_",dates[i]));print(paste0("lbst_OO_",dates[i]))
    lbst_OOm1=get(paste0("lbst_OO_",dates[i]-1));print(paste0("lbst_OO_",dates[i]-1))
    eco_lag=get(paste0("EcoCast_Lagged_CHL_1_",dates[i]));print(paste0("EcoCast_Lagged_CHL_1_",dates[i]))
    eco_OO=get(paste0("EcoCast_OO_",dates[i]))
    eco_OOm1=get(paste0("EcoCast_OO_",dates[i]-1))
    lbst_lag=abs(lbst_OO-lbst_lag)
    lbst_OOd=abs(lbst_OO-lbst_OOm1)
    eco_lag=abs(eco_OO-eco_lag)
    eco_OOd=abs(eco_OO-eco_OOm1)
    q=cellStats(lbst_lag,sum)/11424
    r=cellStats(lbst_OOd,sum)/11424
    s=cellStats(eco_lag,sum)/11424
    t=cellStats(eco_OOd,sum)/11424
    empty[i,1]=as.character(dates[i])
    empty[i,2]=q
    empty[i,3]=r
    empty[i,4]=s
    empty[i,5]=t
  }
  
  a=melt(empty,id.vars = "time")
  a=a[complete.cases(a),]
  a$time=as.factor(a$time)
  
  master=ggplot()+geom_line(data=a,aes(x=time,y=value,color=variable,group=variable))
  ms2=master+labs(y="Mean of per pixel difference from zero lag")+ggtitle("Comparison of lagged and OO analyses for first two weeks of Aug 2012")+
    theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ 
    theme(text = element_text(size=15))+ theme(legend.position="bottom",legend.key = element_blank())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))+
    theme(plot.title = element_text(hjust = 0.5)) 
  
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/daily_means_sum_np.png",width=1400,height=600,units='px',pointsize=100)
  ms2
  dev.off()
  
  
  ###############################################################################################
  ############################ check 1d lay 12.15.2017 SST
  dates=seq(as.Date("2012-12-01"),as.Date("2012-12-03"),by = "day")
  studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
  
  bJFMA1=matrix(c(-140,25,  ## define SST box
                  -140,50,
                  -110,50,
                  -110,25,
                  -140,25),
                ncol=2,byrow = T)
  p=Polygon(bJFMA1)
  ps=Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  plot(sps,add=T)
  
  e=extent(sps)
  rm(p,ps,sps)
  
  
  ### read in SST
  lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    ev_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/"
    date_dir=paste0(ev_dir,date,"/analysed_sst.grd")
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("SST_",date)
    assign(name,b)
  }
  ### read in lbst lagged SST
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Lagged_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/Lagged"
    date_dir=paste0(paste0(Lagged_dir_lbst,"/lbst_pa_",dates[i],"_SST_1_mean.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("lbst_SST_1dlag",date)
    assign(name,b)
  }
  
  ### read in lbst OO
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    OO_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/OO"
    date_dir=paste0(paste0(OO_dir_lbst,"/lbst_pa_",dates[i],"__mean.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("lbst_OO_",date)
    assign(name,b)
  }
  
  ### read in ecocast lagged SST
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged"
    date_dir=paste0(paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_SST_1.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EcoCast_Lagged_SST_1_",date)
    assign(name,b)
  }
  
  ### read in ecocast OO
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Ecocast_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO"
    date_dir=paste0(paste0(Ecocast_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EcoCast_OO_",date)
    assign(name,b)
  }
  
  
  myTheme_CHLA=rasterTheme(region=brewer.pal('RdBu', n=11))
  myTheme_lbst=rasterTheme(region=brewer.pal('PiYG', n=11))
  myTheme_EcoLagged=rasterTheme(region=brewer.pal('PRGn', n=11))
  #myTheme_Eco_OO=rasterTheme(region=brewer.pal('RdYlBu', n=11))
  
  #matrix 1: rows="2012-12-01" "2012-12-02" "2012-12-03", cols= CHLA, lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/SST_12.01-12.03.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`SST_2012-12-01`,main="SST, 2012-12-01",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_SST_1dlag2012-12-01`,main="Lbst SST1D, 2012-12-01",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-12-01`,main="Lbst_OO, 2012-12-01",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_SST_1_2012-12-01`,main="EcoLag SST1D, 2012-12-01",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-12-01`,main="Eco_OO, 2012-12-01",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`SST_2012-12-02`,main="SST, 2012-12-02",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_SST_1dlag2012-12-02`,main="Lbst SST1D, 2012-12-02",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-12-02`,main="Lbst_OO, 2012-12-02",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_SST_1_2012-12-02`,main="EcoLag SST1D, 2012-12-02",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-12-02`,main="Eco_OO, 2012-12-02",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`SST_2012-12-03`,main="SST, 2012-12-03",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_SST_1dlag2012-12-03`,main="Lbst SST1D, 2012-12-03",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-12-03`,main="Lbst_OO, 2012-12-03",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_SST_1_2012-12-03`,main="EcoLag SST1D, 2012-12-03",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-12-03`,main="Eco_OO, 2012-12-03",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  ###############################################################################################
  ############################ check 1d lay 12.15.2017 EKE
  dates=seq(as.Date("2012-12-01"),as.Date("2012-12-03"),by = "day")
  studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
  
  bJFMA1=matrix(c(-140,25,  ## define SST box
                  -140,50,
                  -110,50,
                  -110,25,
                  -140,25),
                ncol=2,byrow = T)
  p=Polygon(bJFMA1)
  ps=Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  plot(sps,add=T)
  
  e=extent(sps)
  rm(p,ps,sps)
  
  
  ### read in EKE
  lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    ev_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/"
    date_dir=paste0(ev_dir,date,"/l.eke_mean.grd")
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EKE_",date)
    assign(name,b)
  }
  ### read in lbst lagged EKE
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Lagged_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/Lagged"
    date_dir=paste0(paste0(Lagged_dir_lbst,"/lbst_pa_",dates[i],"_EKE_1_mean.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("lbst_EKE_1dlag",date)
    assign(name,b)
  }
  
  ### read in lbst OO
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    OO_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/OO"
    date_dir=paste0(paste0(OO_dir_lbst,"/lbst_pa_",dates[i],"__mean.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("lbst_OO_",date)
    assign(name,b)
  }
  
  ### read in ecocast lagged EKE
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged"
    date_dir=paste0(paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_EKE_1.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EcoCast_Lagged_EKE_1_",date)
    assign(name,b)
  }
  
  ### read in ecocast OO
  #lss=list()
  for(i in 1:length(dates)){
    print(i)
    date=as.character(dates[i])
    print(date)
    Ecocast_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO"
    date_dir=paste0(paste0(Ecocast_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_.grd"))
    #lss=list(lss,date_dir)
    a=raster(date_dir)*studyarea
    b=crop(a,e)
    name=paste0("EcoCast_OO_",date)
    assign(name,b)
  }
  
  
  myTheme_CHLA=rasterTheme(region=brewer.pal('RdBu', n=11))
  myTheme_lbst=rasterTheme(region=brewer.pal('PiYG', n=11))
  myTheme_EcoLagged=rasterTheme(region=brewer.pal('PRGn', n=11))
  #myTheme_Eco_OO=rasterTheme(region=brewer.pal('RdYlBu', n=11))
  
  #matrix 1: rows="2012-12-01" "2012-12-02" "2012-12-03", cols= CHLA, lbst, EcoLagged, Eco_OO
  png("/Volumes/SeaGate/DOM_sensitivity/checking_lags/EKE_12.01-12.03.png",
      width     = 18,
      height    = 12,
      units     = "in",
      res       = 1200)
  a=levelplot(`EKE_2012-12-01`,main="EKE, 2012-12-01",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  b=levelplot(`lbst_EKE_1dlag2012-12-01`,main="Lbst EKE1D, 2012-12-01",par.settings=myTheme_lbst,margin=F,colorkey=F)
  bb=levelplot(`lbst_OO_2012-12-01`,main="Lbst_OO, 2012-12-01",par.settings=myTheme_lbst,margin=F,colorkey=F)
  c=levelplot(`EcoCast_Lagged_EKE_1_2012-12-01`,main="EcoLag EKE1D, 2012-12-01",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  d=levelplot(`EcoCast_OO_2012-12-01`,main="Eco_OO, 2012-12-01",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  e=levelplot(`EKE_2012-12-02`,main="EKE, 2012-12-02",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  f=levelplot(`lbst_EKE_1dlag2012-12-02`,main="Lbst EKE1D, 2012-12-02",par.settings=myTheme_lbst,margin=F,colorkey=F)
  ff=levelplot(`lbst_OO_2012-12-02`,main="Lbst_OO, 2012-12-02",par.settings=myTheme_lbst,margin=F,colorkey=F)
  g=levelplot(`EcoCast_Lagged_EKE_1_2012-12-02`,main="EcoLag EKE1D, 2012-12-02",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  h=levelplot(`EcoCast_OO_2012-12-02`,main="Eco_OO, 2012-12-02",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  i=levelplot(`EKE_2012-12-03`,main="EKE, 2012-12-03",par.settings=myTheme_CHLA,margin=F,colorkey=F)
  j=levelplot(`lbst_EKE_1dlag2012-12-03`,main="Lbst EKE1D, 2012-12-03",par.settings=myTheme_lbst,margin=F,colorkey=F)
  jj=levelplot(`lbst_OO_2012-12-03`,main="Lbst_OO, 2012-12-03",par.settings=myTheme_lbst,margin=F,colorkey=F)
  k=levelplot(`EcoCast_Lagged_EKE_1_2012-12-03`,main="EcoLag EKE1D, 2012-12-03",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  l=levelplot(`EcoCast_OO_2012-12-03`,main="Eco_OO, 2012-12-03",par.settings=myTheme_EcoLagged,margin=F,colorkey=F)
  
  grid.arrange(a,b,bb,c,d,e,f,ff,g,h,i,j,jj,k,l,ncol=5)
  dev.off()
  
  
  