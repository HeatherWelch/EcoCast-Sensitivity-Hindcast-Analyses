########## Figure 1 ------> Two part figure: a. shows the eastern pacific with an inset for the DGN and PLCA, b. show a sample EcoCast output
library(sp)
library(rgdal)
library(raster)
library(sp)
library(tidyverse)
library(rworldmap)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(ggmap)
library(grid)
library(lattice)
library(gridExtra)

#-----> ###### Fig. 1a ######
data_dir="~/Dropbox/spatial_data"
EEZ=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="US_EEZ")
EEZ=EEZ[EEZ@data$Sovereign=="United States",]
EEZ=as.data.frame(EEZ)
EEZ=EEZ[EEZ$Sovereign=="United States",]
USA=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="cb_2014_us_nation_5m")
PLCA=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="LeatherbackClosure")

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_polygon(data=EEZ,aes(x=Longitude,y=Latitude,fill="EEZ"))+coord_cartesian()


#####

########## Figure 1 ------> Two part figure: a. shows the eastern pacific with an inset for the DGN and PLCA, b. show a sample EcoCast output
library(sp)
library(rgdal)
library(raster)
library(sp)
library(tidyverse)
library(rworldmap)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(ggmap)
library(grid)
library(lattice)
library(gridExtra)

#-----> ###### Fig. 1a ######
data_dir="~/Dropbox/spatial_data"
EEZ=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="US_EEZ")
EEZ=EEZ[EEZ@data$Sovereign=="United States",]
EEZ=as.data.frame(fortify(EEZ,region="Sovereign"))
USA=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="cb_2014_us_nation_5m")
PLCA=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="LeatherbackClosure")

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_polygon(data=EEZ,aes(x=Longitude,y=Latitude,fill="EEZ"))+coord_cartesian()


#####
########## Figure 1 ------> Two part figure: a. shows the eastern pacific with an inset for the DGN and PLCA, b. show a sample EcoCast output
library(sp)
library(rgdal)
library(raster)
library(sp)
library(tidyverse)
library(rworldmap)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(ggmap)
library(grid)
library(lattice)
library(gridExtra)

#-----> ###### Fig. 1a ######
data_dir="~/Dropbox/spatial_data"
EEZ=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="US_EEZ")
EEZ=EEZ[EEZ@data$Sovereign=="United States",]
EEZ=as.data.frame(fortify(EEZ,region="Sovereign"))
USA=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="cb_2014_us_nation_5m")
PLCA=readOGR(dsn=path.expand("~/Dropbox/spatial_data"),layer="LeatherbackClosure")

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_polygon(data=EEZ,aes(x=long,y=lat,fill="EEZ"))+coord_cartesian()


#####

