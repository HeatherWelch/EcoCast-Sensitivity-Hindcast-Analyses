### thresholding sensitivity
# natural elbows were not apparent in the 4 difference metrics
# trying a threshold: when mean value is > mean ecocast value +/- SD

###1. find mean ecocast value
###2. find spatial SD ecocast value
###3. find mean value for each sensitivity layer
###4. plot w threshold line

library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)

OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

###1. find mean ecocast value, ####2. find spatial SD ecocast value
ecocast=list.files(OO_dir,pattern=".grd$",full.names = T)
c=grep("_2012-",ecocast,value = T)
d=grep("_2015-",ecocast,value = T)
e=unlist(list(c,d))
ecostack=stack(e)
clip_stack=ecostack*studyarea
means=calc(clip_stack, mean)
r=cellStats(means,mean) # spatial sum r=0.4433651
s=cellStats(means,sd) # spatial standard deviation s=0.2055143

###3. find mean value for each sensitivity layer
