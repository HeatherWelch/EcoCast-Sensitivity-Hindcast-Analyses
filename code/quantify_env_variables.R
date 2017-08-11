#### LAGGED RAW VARIABLE SENSITIVITY #########

### code to see how much the variables themselves vary over time

rasterRescale<-function(r){
  ((r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min")))
}

####### general objects
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")
var_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/"

ecocast=list.files(OO_dir,pattern=".grd$",full.names = T)
c=grep("_2012-",ecocast,value = T)
d=grep("_2015-",ecocast,value = T)
e=unlist(list(c,d))
a=unlist(lapply(e,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x)))
b=unlist(lapply(a,function(x)gsub("_.grd","",x)))


layers=c("analysed_sst","l.blendChl","l.eke_mean","sla","ywind")
lags=c(1,7,14,21,28,30)
var_names=unlist(list("SST","CHLA","EKE","YWIND","SLA"))
empty=data.frame(month=NA,day=NA,year=NA,missing_var=NA,s.mean=NA,s.SD=NA,p.GT.5=NA,p.GT.25=NA,p.GT.1=NA,lag=NA)

#pos_list=unlist(list(seq(1:150),(153:length(b))))
pos_list=unlist(list(seq(from=160,to=306)))
for(i in pos_list){ ##missing 152, and start of 2015 (missing 12/29, 12/30) #153:length(b), 172:length(b)
  #for(i in 160:length(b)){
  OO_date=b[i]
  print(OO_date)
  for(iii in 1:length(lags)){
  lag_date=as.Date(b[i])-lags[iii]
  print(lag_date)
  if(lag_date>as.Date(b[1])){
    
    print(paste0("lag is ",lags[iii]))
    SST=paste0(var_dir,OO_date,"/analysed_sst.grd")
    CHLA=paste0(var_dir,OO_date,"/l.blendChl.grd")
    EKE=paste0(var_dir,OO_date,"/l.eke_mean.grd")
    SLA=paste0(var_dir,OO_date,"/sla.grd")
    YWIND=paste0(var_dir,OO_date,"/ywind.grd")
    
    SST_lag=paste0(var_dir,lag_date,"/analysed_sst.grd")
    CHLA_lag=paste0(var_dir,lag_date,"/l.blendChl.grd")
    EKE_lag=paste0(var_dir,lag_date,"/l.eke_mean.grd")
    SLA_lag=paste0(var_dir,lag_date,"/sla.grd")
    YWIND_lag=paste0(var_dir,lag_date,"/ywind.grd")
    
    vars=unlist(list(SST,CHLA,EKE,YWIND,SLA))
    var_list=stack(vars)*studyarea
    vars_lag=unlist(list(SST_lag,CHLA_lag,EKE_lag,YWIND_lag,SLA_lag))
    var_list_lag=stack(vars_lag)*studyarea
    
    for(ii in 1:length(vars)){
      q=abs(rasterRescale(var_list[[ii]])-rasterRescale(var_list_lag[[ii]]))
      r=cellStats(q,sum)/12936 # spatial sum
      s=cellStats(q,sd)/12936 # spatial standard deviation
      t=cellStats(q>.5,sum)/12936 # % cells where difference > .5
      u=cellStats(q>.25,sum)/12936 # % cells where difference > .25
      v=cellStats(q>.1,sum)/12936 # % cells where difference > .1
      empty[30*i+(ii*6),1]=substring(OO_date, first=6, last = 7)
      empty[30*i+(ii*6),2]=substring(OO_date, first=9, last = 10)
      empty[30*i+(ii*6),3]=substring(OO_date, first=1, last = 4)
      empty[30*i+(ii*6),4]=var_names[ii]
      empty[30*i+(ii*6),5]=r
      empty[30*i+(ii*6),6]=s
      empty[30*i+(ii*6),7]=t
      empty[30*i+(ii*6),8]=u
      empty[30*i+(ii*6),9]=v
      empty[30*i+(ii*6),10]=lags[iii]
    }
  }
  }
}

DF_complete2=empty[complete.cases(empty),]
df=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_envt_vars.csv")
df=df[,2:ncol(df)]
DF_complete=rbind(df,DF_complete2)
#write.csv(DF_complete,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_envt_vars.csv")  ##this one has 2012 and 2015
#write.csv(df,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_envt_vars_2012.csv") ## this one only has 2012

####### make some plots, averaged across month
DF_complete=DF_complete[,c(4:10)]
##make %
DF_complete$p.GT.1=DF_complete$p.GT.1*100
DF_complete$p.GT.25=DF_complete$p.GT.25*100
DF_complete$p.GT.5=DF_complete$p.GT.5*100

a=melt(DF_complete,id=c("missing_var","lag"))

means=cast(a,lag~missing_var,mean,subset=variable=="s.mean")
means_s.mean=melt(means,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="s.SD")
means_s.SD=melt(means,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="p.GT.5")
means_p.GT.5=melt(means,id=c("missing_var","lag"))
means=cast(a,missing_var~lag,mean,subset=variable=="p.GT.1")
means_p.GT.1=melt(means,id=c("missing_var","lag"))

###plotting
s.mean=ggplot(means_s.mean, aes(lag, value,color=missing_var))+ geom_line(linetype=2) + geom_point(color="blue", size=3)+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables") + expand_limits(y=0)
a=s.mean+labs(x="Number of days lagged")+labs(y="Mean per pixel difference from zero lag")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())

s.SD=ggplot(means_s.SD, aes(lag, value,color=missing_var))+ geom_line(linetype=2) + geom_point(color="blue", size=3)+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables")+ expand_limits(y=0)
b=s.SD+labs(x="Number of days lagged")+labs(y="Standard deviation of per pixel difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())


p.GT.5=ggplot(means_p.GT.5, aes(lag, value,color=missing_var))+ geom_line(linetype=2) + geom_point(color="blue", size=3)+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables")+ expand_limits(y=0)
c=p.GT.5+labs(x="Number of days lagged")+labs(y="% of pixels with > .5 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())


p.GT.1=ggplot(means_p.GT.1, aes(lag, value,color=missing_var))+ geom_line(linetype=2) + geom_point(color="blue", size=3)+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables")+ expand_limits(y=0)
d=p.GT.1+labs(x="Number of days lagged")+labs(y="% of pixels with > .1 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())


png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_analysis_raw_vars.png",width=1100,height=1100,units='px',pointsize=35)
grid.arrange(a,b,c,d,top=textGrob("Lagged raw variable sensitivity analysis",gp=gpar(fontsize=20)))
dev.off()
