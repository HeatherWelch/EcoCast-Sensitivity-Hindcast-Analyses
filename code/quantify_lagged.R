################ LAGGED VARIABLE SENSITIVITY ################

### code to quantify ecocast sensitivity to missing data
## # pixels in SA = 12936 ncell(studyarea)

library(raster)
library(ggplot2)
library(reshape)
library(gridExtra)
library(grid)

####### general objects
Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged"
OO_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO" 
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")
var_names=list("SST","SST_both","SST_SD","CHLA","EKE","ywind","SLA","SLA_both","SLA_SD")
