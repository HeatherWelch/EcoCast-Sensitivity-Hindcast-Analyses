################ LAGGED OFFICIAL OUTPUT SENSITIVITY ####################

### code to quantify ecocast sensitivity to missing data
## # pixels in SA = 12936 ncell(studyarea)

library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)

####### general objects
Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged" 
LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

###### define function
quantify_OO=function(dir=OO_dir,studyarea){
  setwd(dir)
  
  ecocast=list.files(dir,pattern=".grd$",full.names = T)
  c=grep("_2012-",ecocast,value = T)
  ecostack_2012=stack(c)
  a=lapply(c,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x))
  b=unlist(lapply(a,function(x)gsub("_.grd","",x)))
  names(ecostack_2012)=b
  clip_2012=ecostack_2012*studyarea
  names(clip_2012)=b
  
  d=grep("_2015-",ecocast,value = T)
  ecostack_2015=stack(d)
  a=lapply(d,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x))
  b=unlist(lapply(a,function(x)gsub("_.grd","",x)))
  names(ecostack_2015)=b
  clip_2015=ecostack_2015*studyarea
  names(clip_2015)=b
  
  empty_2012=data.frame(month=NA,day=NA,year=NA,t.minus=NA,s.mean=NA,s.SD=NA,p.GT.5=NA,p.GT.25=NA,p.GT.1=NA)
  date_list=c(1:8,14,30)
  
   for(i in 1:nlayers(clip_2012)){
    for(ii in 1:length(date_list)){
      print(ii)
      n=date_list[ii]
      if(i-n>0){
      a=abs(clip_2012[[i]]-clip_2012[[i-n]]) ####### __________________think about this plus minus stuff
      b=cellStats(a,sum)/12936 # spatial sum
      c=cellStats(a,stat='sd')/12936 # spatial standard deviation
      d=cellStats(a>.5,sum)/12936 # % cells where difference > .5
      e=cellStats(a>.25,sum)/12936 # % cells where difference > .25
      f=cellStats(a>.1,sum)/12936 # % cells where difference > .1
      empty_2012[10*i+ii,1]=substring(names(clip_2012[[i]]), first=7, last = 8)
      empty_2012[10*i+ii,2]=substring(names(clip_2012[[i]]), first=10, last = 11)
      empty_2012[10*i+ii,3]=substring(names(clip_2012[[i]]), first=2, last = 5)
      empty_2012[10*i+ii,4]=n
      empty_2012[10*i+ii,5]=b
      empty_2012[10*i+ii,6]=c
      empty_2012[10*i+ii,7]=d
      empty_2012[10*i+ii,8]=e
      empty_2012[10*i+ii,9]=f
    }
    }
   }
  
  empty_2015=data.frame(month=NA,day=NA,year=NA,t.minus=NA,s.mean=NA,s.SD=NA,p.GT.5=NA,p.GT.25=NA,p.GT.1=NA)
  
  for(i in 1:nlayers(clip_2015)){
    for(ii in 1:length(date_list)){
      print(ii)
      n=date_list[ii]
      if(i-n>0){
        a=abs(clip_2015[[i]]-clip_2015[[i-n]])
        b=cellStats(a,sum)/12936 # spatial sum
        c=cellStats(a,stat='sd')/12936 # spatial standard deviation
        d=cellStats(a>.5,sum)/12936 # % cells where difference > .5
        e=cellStats(a>.25,sum)/12936 # % cells where difference > .25
        f=cellStats(a>.1,sum)/12936 # % cells where difference > .1
        empty_2015[10*i+ii,1]=substring(names(clip_2015[[i]]), first=7, last = 8)
        empty_2015[10*i+ii,2]=substring(names(clip_2015[[i]]), first=10, last = 11)
        empty_2015[10*i+ii,3]=substring(names(clip_2015[[i]]), first=2, last = 5)
        empty_2015[10*i+ii,4]=n
        empty_2015[10*i+ii,5]=b
        empty_2015[10*i+ii,6]=c
        empty_2015[10*i+ii,7]=d
        empty_2015[10*i+ii,8]=e
        empty_2015[10*i+ii,9]=f
      }
    }
  }
  
  OO=rbind(empty_2012,empty_2015)
  return(OO)
  
}

###### run function
DF=quantify_OO(dir=OO_dir,studyarea=studyarea)
#write.csv(DF,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO.csv")
#DF=read.csv("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO.csv")

####### make some plots, averaged across month
DF_complete=DF[complete.cases(DF),]
DF_complete=DF_complete[,c(4:10)]
a=melt(DF_complete,id="t.minus")
means=cast(a,t.minus~variable,mean)
means=cast(a,t.minus~variable,c(mean,sd))

##make %
means$p.GT.1=means$p.GT.1_mean*100
means$p.GT.25=means$p.GT.25_mean*100
means$p.GT.5=means$p.GT.5_mean*100

## thinking about error bars
means$upper=means$s.mean_mean+means$s.mean_sd
means$lower=means$s.mean_mean-means$s.mean_sd
means$s.mean=means$s.mean_mean
means$s.SD=means$s.SD_mean

# #normalizing between 1:0
# range01 <- function(x){(x-min(x))/(max(x)-min(x))} 
# means01=means
# means01=as.data.frame(lapply(means01,FUN=range01))
# means01$t.minus=means$t.minus

##### adding SD cuttoff (SD is the spatial standard deviation across the entire official stack)
# spatial standard deviation s=0.2055143
# cutoff <- data.frame( x = c(-Inf, Inf), y =0.2055143, cutoff = factor(0.2055143) )

###plotting
s.mean=ggplot(means, aes(t.minus, s.mean)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)+ expand_limits(y=0)
a=s.mean+labs(x="Number of days lagged")+labs(y="Mean per pixel difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

# with shaded error
# s.mean=ggplot(means, aes(t.minus, s.mean)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)+ expand_limits(y=0) +geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)
# a=s.mean+labs(x="Number of days lagged")+labs(y="Mean per pixel difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

s.SD=ggplot(means, aes(t.minus, s.SD)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)+ expand_limits(y=0)
b=s.SD+labs(x="Number of days lagged")+labs(y="Standard deviation of per pixel difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

p.GT.5=ggplot(means, aes(t.minus, p.GT.5)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)+ expand_limits(y=0)
c=p.GT.5+labs(x="Number of days lagged")+labs(y="% of pixels with > .5 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

p.GT.1=ggplot(means, aes(t.minus, p.GT.1)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)+ expand_limits(y=0)
d=p.GT.1+labs(x="Number of days lagged")+labs(y="% of pixels with > .1 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO_analysis.png",width=1100,height=1100,units='px',pointsize=35)
grid.arrange(a,b,c,d,top=textGrob("Lagged official output analysis",gp=gpar(fontsize=20)))
dev.off()



########################################################################################################################################################## (abandon, can be correlated while still having a large difference)
###################### correlation (abandon, can be correlated while still having a large difference)
# quantify_OO_lagged=function(dir=OO_dir,studyarea){
#   setwd(dir)
#   
#   ecocast=list.files(dir,pattern=".grd$",full.names = T)
#   c=grep("_2012-",ecocast,value = T)
#   ecostack_2012=stack(c)
#   a=lapply(c,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x))
#   b=unlist(lapply(a,function(x)gsub("_.grd","",x)))
#   names(ecostack_2012)=b
#   clip_2012=ecostack_2012*studyarea
#   names(clip_2012)=b
#   
#   d=grep("_2015-",ecocast,value = T)
#   ecostack_2015=stack(d)
#   a=lapply(d,function(x)gsub("/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_","",x))
#   b=unlist(lapply(a,function(x)gsub("_.grd","",x)))
#   names(ecostack_2015)=b
#   clip_2015=ecostack_2015*studyarea
#   names(clip_2015)=b
#   
#   empty_2012=data.frame(month=NA,day=NA,year=NA,t.minus=NA,corellation=NA,pvalue=NA,p.GT.9.cor=NA,p.GT.05.pval=NA)
#   date_list=c(1:8,14,30)
#   
#   for(i in 1:nlayers(clip_2012)){
#     for(ii in 1:length(date_list)){
#       print(ii)
#       n=date_list[ii]
#       if(i+n<nlayers(clip_2012)){
#         r=corLocal(clip_2012[[i]],clip_2012[[i+n]],test=T)
#         r=r*studyarea
#         cor_values=getValues(r[[1]])
#         pval_values=getValues(r[[2]])
#         cor=cellStats(r[[1]],mean)
#         pval=cellStats(r[[2]],mean) # spatial standard deviation
#         empty_2012[10*i+ii,1]=substring(names(clip_2012[[i]]), first=7, last = 8)
#         empty_2012[10*i+ii,2]=substring(names(clip_2012[[i]]), first=10, last = 11)
#         empty_2012[10*i+ii,3]=substring(names(clip_2012[[i]]), first=2, last = 5)
#         empty_2012[10*i+ii,4]=n
#         empty_2012[10*i+ii,5]=cor
#         empty_2012[10*i+ii,6]=pval
#         empty_2012[10*i+ii,7]=length(cor_values[cor_values>.9])/12936*100
#         empty_2012[10*i+ii,8]=length(pval_values[pval_values<.05])/12936*100
#       }
#     }
#   }
#   
#   empty_2015=data.frame(month=NA,day=NA,year=NA,t.minus=NA,corellation=NA,pvalue=NA,p.GT.9.cor=NA,p.GT.05.pval=NA)
#   
#   for(i in 1:nlayers(clip_2015)){
#     for(ii in 1:length(date_list)){
#       print(ii)
#       n=date_list[ii]
#       if(i+n<nlayers(clip_2015)){
#         r=corLocal(clip_2015[[i]],clip_2015[[i+n]],test=T)
#         r=r*studyarea
#         cor_values=getValues(r[[1]])
#         pval_values=getValues(r[[2]])
#         cor=cellStats(r[[1]],mean)
#         pval=cellStats(r[[2]],mean) # spatial standard deviation
#         empty_2015[10*i+ii,1]=substring(names(clip_2015[[i]]), first=7, last = 8)
#         empty_2015[10*i+ii,2]=substring(names(clip_2015[[i]]), first=10, last = 11)
#         empty_2015[10*i+ii,3]=substring(names(clip_2015[[i]]), first=2, last = 5)
#         empty_2015[10*i+ii,4]=n
#         empty_2015[10*i+ii,5]=cor
#         empty_2015[10*i+ii,6]=pval
#         empty_2015[10*i+ii,7]=length(cor_values[cor_values>.9])/12936*100
#         empty_2015[10*i+ii,8]=length(pval_values[pval_values<.05])/12936*100
#       }
#     }
#   }
#   
#   OO=rbind(empty_2012,empty_2015)
#   return(OO)
#   
# }
# 
# DF=quantify_OO_lagged(dir=OO_dir,studyarea=studyarea)
# write.csv(DF,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO_cor.csv")
# 
# ####### make some plots, averaged across month
# DF_complete=DF[complete.cases(DF),]
# DF_complete=DF_complete[,c(4:8)]
# a=melt(DF_complete,id="t.minus")
# means=cast(a,t.minus~variable,mean)
# 
# ###plotting
# s.mean=ggplot(means, aes(t.minus, corellation)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
# a=s.mean+labs(x="Number of days lagged")+labs(y="Mean per pixel correlation with zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))
# 
# s.SD=ggplot(means, aes(t.minus, pvalue)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
# b=s.SD+labs(x="Number of days lagged")+labs(y="Mean per pixel correlation p-value with zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))
# 
# p.GT.5=ggplot(means, aes(t.minus, p.GT.9.cor)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
# c=p.GT.5+labs(x="Number of days lagged")+labs(y="% of pixels with > .9 correlation with zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))
# 
# p.GT.1=ggplot(means, aes(t.minus, p.GT.05.pval)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
# d=p.GT.1+labs(x="Number of days lagged")+labs(y="% of pixels with < .05 correlation p-value with zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))
# 
# png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO_analysis_cor.png",width=1100,height=1100,units='px',pointsize=35)
# grid.arrange(a,b,c,d,top=textGrob("Lagged official output analysis",gp=gpar(fontsize=20)))
# dev.off()
