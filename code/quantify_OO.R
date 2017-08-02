### code to quantify ecocast sensitivity to missing data
## # pixels in SA = 12936 ncell(studyarea)

library(raster)

#######commands

Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged" 
LOO="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/LOO" 
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

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
  
}
