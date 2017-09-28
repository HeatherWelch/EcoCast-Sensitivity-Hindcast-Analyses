########## Figure 1 ------> Two part figure: a. shows the eastern pacific with an inset for the DGN and PLCA, b. show a sample EcoCast output
library(sp)
library(rgdal)
library(raster)


#-----> ###### Fig. 1a ######
data_dir="/Volumes/SeaGate/Operationalization_Sensitivity/spatial_data"
usa=readOGR(dsn=path.expand(paste0(data_dir,"/LeatherbackClosure")),layer="LeatherbackClosure")
usa=shapefile(paste0(data_dir,"/cb_2014_us_nation_5m.shp"))
usa=readOGR(dsn=data_dir, layer="US_EEZ")

test=readOGR(dsn=data_dir,layer="US_EEZ")
#####