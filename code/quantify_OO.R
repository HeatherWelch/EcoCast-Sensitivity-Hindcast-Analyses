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
      if(i+n<nlayers(clip_2012)){
      a=abs(clip_2012[[i]]-clip_2012[[i+n]])
      b=cellStats(a,sum) # spatial sum
      c=cellStats(a,sd) # spatial standard deviation
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
      if(i+n<nlayers(clip_2015)){
        a=abs(clip_2015[[i]]-clip_2015[[i+n]])
        b=cellStats(a,sum) # spatial sum
        c=cellStats(a,sd) # spatial standard deviation
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
write.csv(DF,"/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO.csv")

####### make some plots, segraged by month
plot(DF$t.minus,DF$s.mean,cols=DF$month)
qplot(t.minus, s.mean, colour = month,
      data = DF)

ggplot(DF, aes(t.minus, s.mean,colour = month)) + geom_point() + geom_smooth(method = "lm")
ggplot(DF, aes(t.minus, s.SD,colour = month)) + geom_point() + geom_smooth(method = "lm")
ggplot(DF, aes(t.minus, p.GT.5,colour = month)) + geom_point() + geom_smooth(method = "lm")
ggplot(DF, aes(t.minus, p.GT.25,colour = month)) + geom_point() + geom_smooth(method = "lm")
ggplot(DF, aes(t.minus, p.GT.1,colour = month)) + geom_point() + geom_smooth(method = "lm")

####### make some plots, averaged across month
DF_complete=DF[complete.cases(DF),]
DF_complete=DF_complete[,c(4:9)]
a=melt(DF_complete,id="t.minus")
means=cast(a,t.minus~variable,mean)

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
s.mean=ggplot(means, aes(t.minus, s.mean)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
a=s.mean+labs(x="Number of days lagged")+labs(y="Mean difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

s.SD=ggplot(means, aes(t.minus, s.SD)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
b=s.SD+labs(x="Number of days lagged")+labs(y="SD of difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

p.GT.5=ggplot(means, aes(t.minus, p.GT.5)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
c=p.GT.5+labs(x="Number of days lagged")+labs(y="% of pixels with > .5 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

p.GT.1=ggplot(means, aes(t.minus, p.GT.1)) + geom_point() + geom_line(colour="blue")+geom_text(aes(label=t.minus),hjust=2)
d=p.GT.1+labs(x="Number of days lagged")+labs(y="% of pixels with > .1 difference from zero lag")+ theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=15))

png("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_Sensitivity_Hindcast/EcoCast-Sensitivity-Hindcast-Analyses/analysis_DFs/OO_analysis.png",width=1100,height=1100,units='px',pointsize=35)
grid.arrange(a,b,c,d,top=textGrob("Lagged official output analysis",gp=gpar(fontsize=20)))
dev.off()

