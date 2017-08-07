################ LEAVE ONE OUT ANALYSIS SENSITIVITY ################

### code to quantify ecocast sensitivity to missing data
## # pixels in SA = 12936 ncell(studyarea)

library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)

####### general objects
LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
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
empty=data.frame(month=NA,day=NA,year=NA,missing_var=NA,s.mean=NA,s.SD=NA,p.GT.5=NA,p.GT.25=NA,p.GT.1=NA)

######## run quantification
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")
pos_list=unlist(list(seq(1:150),(153:length(b))))
for(i in pos_list){ #152 (missing 12/29, 12/30)
  print(i)
  print(b[i])
  OO=clip_stack[[i]]
  SST=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SST.grd")
  SST_both=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SST_both.grd")
  SST_SD=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SST_SD.grd")
  CHLA=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_CHLA.grd")
  EKE=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_EKE.grd")
  SLA=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SLA.grd")
  SLA_both=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SLA_both.grd")
  SLA_SD=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_SLA_SD.grd")
  ywind=paste0(LOO,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",b[i],"_ywind.grd")
  vars=unlist(list(SST,SST_both,SST_SD,CHLA,EKE,ywind,SLA,SLA_both,SLA_SD))
  var_list=stack(vars)
  for(ii in 1:length(vars)){
    print(vars[ii])
    q=abs(OO-var_list[[ii]])
    r=cellStats(q,sum)/12936 # spatial sum
    s=cellStats(q,sd)/12936 # spatial standard deviation
    t=cellStats(q>.5,sum)/12936 # % cells where difference > .5
    u=cellStats(q>.25,sum)/12936 # % cells where difference > .25
    v=cellStats(q>.1,sum)/12936 # % cells where difference > .1
    empty[9*i+ii,1]=substring(names(clip_stack[[i]]), first=7, last = 8)
    empty[9*i+ii,2]=substring(names(clip_stack[[i]]), first=10, last = 11)
    empty[9*i+ii,3]=substring(names(clip_stack[[i]]), first=2, last = 5)
    empty[9*i+ii,4]=var_names[ii]
    empty[9*i+ii,5]=r
    empty[9*i+ii,6]=s
    empty[9*i+ii,7]=t
    empty[9*i+ii,8]=u
    empty[9*i+ii,9]=v
  }
}

######## write out csv
DF_complete=empty[complete.cases(empty),]
#write.csv(DF_complete,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/LOO.csv")

####### make some plots, averaged across month
DF_complete=DF_complete[,c(4:9)]
a=melt(DF_complete,id="missing_var")
means=cast(a,missing_var~variable,mean)
means=means[c(2,9,1,6,8,7,3,5,4),]

##make %
means$p.GT.1=means$p.GT.1*100
means$p.GT.25=means$p.GT.25*100
means$p.GT.5=means$p.GT.5*100

#normalizing between 1:0
range01 <- function(x){(x-min(x))/(max(x)-min(x))} 
means01=means
means01=as.data.frame(lapply(means01,FUN=range01))
means01$t.minus=means$t.minus

###plotting
s.mean=ggplot(means, aes(missing_var, s.mean)) + geom_col() + expand_limits(y=0)
a=s.mean+labs(x="Missing variable")+labs(y="Mean per pixel difference from no missing variables")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=13))

s.SD=ggplot(means, aes(missing_var, s.SD)) + geom_col() + expand_limits(y=0)
b=s.SD+labs(x="Missing variable")+labs(y="Standard deviation of per pixel difference from no missing variables")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=13))

p.GT.5=ggplot(means, aes(missing_var, p.GT.5)) + geom_col() + expand_limits(y=0)
c=p.GT.5+labs(x="Missing variable")+labs(y="% of pixels with > .5 difference from no missing variables")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=13))

p.GT.1=ggplot(means, aes(missing_var, p.GT.1)) + geom_col() + expand_limits(y=0)
d=p.GT.1+labs(x="Missing variable")+labs(y="% of pixels with > .1 difference from no missing variables")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=13))

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/LOO_analysis.png",width=1100,height=1100,units='px',pointsize=35)
grid.arrange(a,b,c,d,top=textGrob("Leave one out analysis",gp=gpar(fontsize=20)))
dev.off()


