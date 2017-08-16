#### LAGGED HABITAT SUITABILITY SENSITIVITY #########

### code to see how much the habitat suitability layers vary over time

########## --------------- LEATHERBACK SEA TURTLES
library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)
library(reshape2)

####### general objects
Lagged_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/Lagged"
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

######## set up data
ecocast=list.files(OO_dir,pattern=".grd$",full.names = T)
e=grep("mean",ecocast,value = T)
ecostack=stack(e)
clip_stack=ecostack*studyarea

a=unlist(lapply(e,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/OO/lbst_pa_","",x)))
b=unlist(lapply(a,function(x)gsub("__mean.grd","",x)))
names(ecostack)=b
names(clip_stack)=b
empty=data.frame(month=NA,day=NA,year=NA,missing_var=NA,s.mean=NA,s.SD=NA,p.GT.5=NA,p.GT.25=NA,p.GT.1=NA,lag=NA)

######## run quantification
# i=date
# ii=variable for date
# iii=lag for variable for date

var_names=unlist(list("SST","CHLA","EKE","YWIND","SLA"))
lags=c(1,7,14,21,28,30)
#pos_list=unlist(list(seq(1:150),(153:length(b))))
pos_list=unlist(list(seq(from=160,to=306)))
for(i in pos_list){ ##missing 152, and start of 2015 (missing 12/29, 12/30) #153:length(b), 172:length(b)
  print(e[i])
  print(b[i])
  OO=clip_stack[[i]]
  for(iii in 1:length(lags)){
    print(paste0("lag is ",lags[iii]))
    SST=paste0(Lagged_dir_lbst,"/lbst_pa_",b[i],"_SST_",lags[iii],"_mean.grd")
    CHLA=paste0(Lagged_dir_lbst,"/lbst_pa_",b[i],"_CHLA_",lags[iii],"_mean.grd")
    EKE=paste0(Lagged_dir_lbst,"/lbst_pa_",b[i],"_EKE_",lags[iii],"_mean.grd")
    SLA=paste0(Lagged_dir_lbst,"/lbst_pa_",b[i],"_SLA_",lags[iii],"_mean.grd")
    YWIND=paste0(Lagged_dir_lbst,"/lbst_pa_",b[i],"_YWIND_",lags[iii],"_mean.grd")
    vars=unlist(list(SST,CHLA,EKE,YWIND,SLA))
    var_list=stack(vars)
    for(ii in 1:length(vars)){
      print(vars[ii])
      q=abs(OO-var_list[[ii]])
      r=cellStats(q,sum)/12936 # spatial sum
      s=cellStats(q,sd)/12936 # spatial standard deviation
      t=cellStats(q>.5,sum)/12936 # % cells where difference > .5
      u=cellStats(q>.25,sum)/12936 # % cells where difference > .25
      v=cellStats(q>.1,sum)/12936 # % cells where difference > .1
      empty[30*i+(ii*6)+iii,1]=substring(names(clip_stack[[i]]), first=7, last = 8)
      empty[30*i+(ii*6)+iii,2]=substring(names(clip_stack[[i]]), first=10, last = 11)
      empty[30*i+(ii*6)+iii,3]=substring(names(clip_stack[[i]]), first=2, last = 5)
      empty[30*i+(ii*6)+iii,4]=var_names[ii]
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
write.csv(DF_complete,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_lbst.csv")

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
s.mean=ggplot(means_s.mean, aes(lag, value,color=missing_var))+ geom_line() + geom_point()+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables") + expand_limits(y=0)
a=s.mean+labs(x="Number of days lagged")+labs(y="Mean per pixel difference from zero lag")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())

s.SD=ggplot(means_s.SD, aes(lag, value,color=missing_var))+ geom_line() + geom_point()+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables")+ expand_limits(y=0)
b=s.SD+labs(x="Number of days lagged")+labs(y="Standard deviation of per pixel difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())


p.GT.5=ggplot(means_p.GT.5, aes(lag, value,color=missing_var))+ geom_line() + geom_point()+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables")+ expand_limits(y=0)
c=p.GT.5+labs(x="Number of days lagged")+labs(y="% of pixels with > .5 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())


p.GT.1=ggplot(means_p.GT.1, aes(lag, value,color=missing_var))+ geom_line() + geom_point()+geom_text(aes(label=lag),show_guide=F,hjust=2)+labs(color = "Missing variables")+ expand_limits(y=0)
d=p.GT.1+labs(x="Number of days lagged")+labs(y="% of pixels with > .1 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))+ theme(legend.key = element_blank())


png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/lagged_lbst.png",width=1100,height=1100,units='px',pointsize=35)
grid.arrange(a,b,c,d,top=textGrob("Lagged LBST sensitivity analysis",gp=gpar(fontsize=20)))
dev.off()

