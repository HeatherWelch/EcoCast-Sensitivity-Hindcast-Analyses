## script to run and plot ecocast from predCIs for all species
## only difference between this script and 5_plot_EcoCast.R in operationalizing_code is this version will not calculate bycatchrisk

Run_ecocast=function(preddate,moddir,griddir,outdir,ecocastdir,sensitivitydir,namesrisk,ecocastrisk,final_path_list){

  ############ 1. load required functions
  
  ## A. rasterRescale
  rasterRescale<-function(r){
    r.min = cellStats(r, "min")
    r.max = cellStats(r, "max")
    r.scale <- ((r - r.min) / (r.max - r.min) - 0.5 ) * 2
    return(r.scale) #(r-rmin)/(rmax-rmin)
  }
  
  ## B. EcoCast_readraster
  EcoCast_readraster<-function(CIobj,yr="2012",griddir,calctype="m",name,sensitivitydir){
    CIdir<-unlist(strsplit(CIobj,"_"))[2]
    
    assign(paste(CIdir,"dir",sep=""),paste(griddir,CIdir,"/predCIs/",sensitivitydir,sep=''))
    varname<-paste(CIdir,"dir",sep="")
    
    allfiles<-list.files(get(varname), glob2rx('*.grd'), full.names=T)
    if (calctype=="m") {
      assign(paste("files",yr,"m",sep=''),allfiles[grep(yr,allfiles)][grep(paste0(name,"_mean"),allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_m_r",sep=''),lapply(get(paste("files",yr,"m",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_m_r",sep='')))
    }
    if (calctype=="se") {
      assign(paste("files",yr,"se",sep=''),allfiles[grep(yr,allfiles)][grep(paste0(name,"_se"),allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_se_r",sep=''),lapply(get(paste("files",yr,"se",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_se_r",sep='')))
    }
    if (calctype=="highCI") {
      assign(paste("files",yr,"hiCI",sep=''),allfiles[grep(yr,allfiles)][grep(paste0(name,"_highCI"),allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_hiCI_r",sep=''),lapply(get(paste("files",yr,"hiCI",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_hiCI_r",sep='')))
    }
    if (calctype=="lowCI") {
      assign(paste("files",yr,"loCI",sep=''),allfiles[grep(yr,allfiles)][grep(paste0(name,"_lowCI"),allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_loCI_r",sep=''),lapply(get(paste("files",yr,"loCI",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_loCI_r",sep='')))
    }
  }
  
  ## C. EcoCalc
  EcoCalc<-function(a,b,c,d,e,risk=ecocastrisk,clipTarget=TRUE){
    ecorisk<-a*risk[1]+b*risk[2]+c*risk[3]+d*risk[4]+e*risk[5]
    if (clipTarget) {
      (ecorisk[(e<0.25)&(ecorisk>0.5)]=100)
    }
    return(ecorisk)
  }
  
  ## D. EcoCols
  EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
  
  ## E. PlotEcoCast
  PlotEcoCast<-function(r,preddate,wd=getwd(),leg=TRUE,scalbar=FALSE,rescal=FALSE,weightings=ecocastrisk,spp=namesrisk,version="_V1",contourval=NA,addLCA=FALSE,zlimits=c(-1,1),addtext=TRUE){
    
    png(paste(wd,"/EcoCast_",paste(weightings,collapse=" "),'_',preddate,"_",final_path_list$name,'.png',sep=''),width=960,height=1100,units='px',pointsize=20)
    par(mar=c(3,3,.5,.5),las=1,font=2)
    if (rescal) r<-rasterRescale(r)
    #zlimits<-c(-1,1)
    if (version=="_se") zlimits<-c(-0.25,0.25)
    if (leg) {
      image.plot(r,col=EcoCols(255),xlim=c(-130,-115),ylim=c(30,48),zlim=zlimits)
    } else {
      image(r,col=EcoCols(255),xlim=c(-130,-115),ylim=c(30,48),zlim=zlimits) ## PRESABS
    }
    if(scalbar) scalebar(110,type="bar", divs=2,below="kilometers")
    if(!is.na(contourval)) {
      SP <- rasterToPolygons(clump(clipLand(r)<(contourval)), dissolve=TRUE)
      plot(SP, add=TRUE)
    }
    if(addLCA) {
      pl <- rbind(c(-121,36.3064), c(-123.583,34.45), c(-129,34.45), c(-129,45), c(-121,45))
      pl <- SpatialPolygons(list(Polygons(list(Polygon(pl)), 1)))
      projection(pl) <- projstring
      plot(pl, border="dark grey", add=TRUE, lty=3, lwd=4)
    }
    
    map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
    if (addtext) {
      text(-122,46.5,preddate,adj=c(0,0),cex=2) 
      text(-122.5,46,"Species weightings",adj=c(0,0),cex=1)
      text(-122,45.5,paste(namesrisk[1],' weighting = ',weightings[1],sep=''),adj=c(0,0),cex=.75)
      text(-122,45,paste(namesrisk[2],' weighting = ',weightings[2],sep=''),adj=c(0,0),cex=.75)
      text(-122,44.5,paste(namesrisk[3],' weighting = ',weightings[3],sep=''),adj=c(0,0),cex=.75)
      text(-122,44,paste(namesrisk[4],' weighting = ',weightings[4],sep=''),adj=c(0,0),cex=.75)
      text(-122,43.5,paste(namesrisk[5],' weighting = ',weightings[5],sep=''),adj=c(0,0),cex=.75)
      
      text(-122.5,43,"Environmental data",adj=c(0,0),cex=1)
      text(-122,42.5,variables_eco[1],adj=c(0,0),cex=.75)
      text(-122,42,variables_eco[2],adj=c(0,0),cex=.75)
      text(-122,41.5,variables_eco[3],adj=c(0,0),cex=.75)
      text(-122,41,variables_eco[4],adj=c(0,0),cex=.75)
      text(-122,40.5,variables_eco[5],adj=c(0,0),cex=.75)
      text(-122,40,variables_eco[6],adj=c(0,0),cex=.75)
      
    }
    box()
    dev.off()
    writeRaster(r,filename=paste(wd,'/EcoCast_',paste(weightings,collapse=" "),"_",preddate,"_",final_path_list$name,'.grd',sep=''),overwrite=TRUE) 
    
    ####### produce netcdf
    lon <- as.array(seq(-149.750622, -99.875, 0.24875622))
    lat <- as.array(seq(60.000622, 10.125, -0.24875622))
    
    z=t(as.matrix(r))
    t=as.numeric(as.POSIXct(preddate))
    
    timedim <- ncdim_def('time', "",as.double(t))
    latdim <- ncdim_def('lat', '', as.double(lat))
    longdim <- ncdim_def('long', '', as.double(lon))
    
    varz <- ncvar_def('EcoCast',"",list(longdim,latdim,timedim),-9999)
    
    nc_name=paste(wd,"/EcoCast_",paste(weightings,collapse=" "),'_',preddate,"_",final_path_list$name,'.nc',sep='')
    outnc=nc_create(nc_name,varz,force_v4=TRUE)
    
    ncvar_put(outnc,varz,z)
    
    ncatt_put(outnc,0,"title",paste0("Official EcoCast output for ",preddate))
    ncatt_put(outnc,0,"description","****some text here****")
    ncatt_put(outnc,0,"date_created",as.character(Sys.time()))
    ncatt_put(outnc,0,"spatial_resolution","0.2487562, 0.2487562  (x, y)")
    ncatt_put(outnc,0,"source_data","ERDDAP (ncdcOwDly_LonPM180, jplG1SST, jplMURSST41), CMEMS (SEALEVEL_GLO_SLA_MAP_L4_NRT_OBSERVATIONS_008_026), AVISO+ (msla)")
    ncatt_put(outnc,0,"Input_var1",variables_eco[1])
    ncatt_put(outnc,0,"Input_var2",variables_eco[2])
    ncatt_put(outnc,0,"Input_var3",variables_eco[3])
    ncatt_put(outnc,0,"Input_var4",variables_eco[4])
    ncatt_put(outnc,0,"Input_var5",variables_eco[5])
    ncatt_put(outnc,0,paste0(namesrisk[1],'_weighting'),weightings[1])
    ncatt_put(outnc,0,paste0(namesrisk[2],'_weighting'),weightings[2])
    ncatt_put(outnc,0,paste0(namesrisk[3],'_weighting'),weightings[3])
    ncatt_put(outnc,0,paste0(namesrisk[4],'_weighting'),weightings[4])
    ncatt_put(outnc,0,paste0(namesrisk[5],'_weighting'),weightings[5])
    ncatt_put(outnc,0,"DISCLAIMER","Draft version only. This product is not intended for on-the-water use.")
    
    ### EcoCast dimension attributes
    ncatt_put(outnc,"EcoCast","ioos_category", "EcoCast")
    ncatt_put(outnc,"EcoCast","long_name", "Relative bycatch:target catch likelihood")
    ncatt_put(outnc,"EcoCast","standard_name","EcoCast")
    ncatt_put(outnc,"EcoCast","units", "NA")
    ncatt_put(outnc,"EcoCast","valid_max", "1")
    ncatt_put(outnc,"EcoCast","valid_min", "-1")
    
    ### time dimension attributes
    ncatt_put(outnc,"time","_CoordinateAxisType", "Time")
    ncatt_put(outnc,"time","actual_range", t)
    ncatt_put(outnc,"time","_actual_range", t)
    ncatt_put(outnc,"time","axis","T")
    ncatt_put(outnc,"time","calendar","Gregorian")
    ncatt_put(outnc,"time","ioos_category", "Time")
    ncatt_put(outnc,"time","long_name", "Start Time")
    ncatt_put(outnc,"time","standard_name","time")
    ncatt_put(outnc,"time","time_origin", "01-JAN-1970 00:00:00")
    ncatt_put(outnc,"time","units", "seconds since 1970-01-01T00:00:00Z")
    
    ### lat dimension attributes
    ncatt_put(outnc,"lat","_CoordinateAxisType", "Lat")
    ncatt_put(outnc,"lat","actual_range", lat[length(lat)])
    ncatt_put(outnc,"lat","_actual_range", lat[1])
    ncatt_put(outnc,"lat","axis","Y")
    ncatt_put(outnc,"lat","ioos_category", "Location")
    ncatt_put(outnc,"lat","long_name", "Latitude")
    ncatt_put(outnc,"lat","standard_name","latitude")
    ncatt_put(outnc,"lat","units", "degrees_north")
    
    ### long dimension attributes
    ncatt_put(outnc,"long","_CoordinateAxisType", "Lon")
    ncatt_put(outnc,"long","actual_range", lon[1])
    ncatt_put(outnc,"long","_actual_range", lon[length(lon)])
    ncatt_put(outnc,"long","axis","X")
    ncatt_put(outnc,"long","ioos_category", "Location")
    ncatt_put(outnc,"long","long_name", "Longitude")
    ncatt_put(outnc,"long","standard_name","Longitude")
    ncatt_put(outnc,"long","units", "degrees_east")
    
    nc_close(outnc)
    
    
  }
  
  ############ 2. Load species confidence interval grids
  CIobjs<-list.files(moddir, glob2rx('*.rds'), full.names=F)
  CIdir<-CIobjs

  for (i in 1:length(CIobjs)) {
    CIdir[i]<-unlist(strsplit(CIobjs[[i]],"_"))[2]
    print(paste("Reading in confidence interval grids for ",CIdir[i],sep=""))
    assign(paste(CIdir[i],preddate,"_m_r",sep=''),EcoCast_readraster(CIobjs[i],yr=preddate,outdir,calctype="m",name=final_path_list$name,sensitivitydir=sensitivitydir))
    assign(paste(CIdir[i],preddate,"_se_r",sep=''),EcoCast_readraster(CIobjs[i],yr=preddate,outdir,calctype="se",name=final_path_list$name,sensitivitydir=sensitivitydir))
  }
  
  mns<-ls()[grep(paste(preddate,"_m_r",sep=""),ls())]
  ses<-ls()[grep(paste(preddate,"_se_r",sep=""),ls())]

  ############ 3. Define coordinate systems
  projstring <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
  oldproj<-CRS("+proj=longlat +datum=WGS84")

  ############ 4. Make list of variable dates
  preddate=preddate
  
  ## first for available variables
  nm=lapply(final_path_list$FileList_final,function(x)unlist(strsplit(x,"/")))
  nm0=lapply(nm,function(x)paste(x[length(x)],x[length(x)-1]))
  variables=as.character(unlist(lapply(nm0,function(x)gsub(".grd","",x[[1]]))))
  variables=gsub("ywind","Surface wind",variables)
  variables=gsub("analysed_sst","Sea surface temperature",variables)
  variables=gsub("l.eke_mean","Eddy kenetic energy",variables)
  variables=gsub("l.blendChl","Chlorophyll a",variables)
  variables=gsub("sla","Sea surface height",variables)
  
  for(var in variables){
    if (grepl("_sd",var)==TRUE){
      variables=variables[!is.element(variables,var)]
    }
    if (grepl("lunillum",var)==TRUE){
      variables=variables[!is.element(variables,var)]
    }
    if (grepl("z",var)==TRUE){
      variables=variables[!is.element(variables,var)]
    }
  }
    
  variables_available=as.character(lapply(variables,function(x)paste(substr(x, 1, nchar(x)-10), "is from", substr(x, nchar(x)-10, nchar(x)), sep = "")))
  
  ## then for missing variables
  variables=lapply(final_path_list$FileList_missing,function(x)gsub(".grd","",x))
  variables=gsub("ywind","Surface wind",variables)
  variables=gsub("analysed_sst","Sea surface temperature",variables)
  variables=gsub("l.eke_mean","Eddy kenetic energy",variables)
  variables=gsub("l.blendChl","Chlorophyll a",variables)
  variables=gsub("sla","Sea surface height",variables)
  
  for(var in variables){
    if (grepl("_sd",var)==TRUE){
      variables=variables[!is.element(variables,var)]
    }
    if (grepl("lunillum",var)==TRUE){
      variables=variables[!is.element(variables,var)]
    }
    if (grepl("z",var)==TRUE){
      variables=variables[!is.element(variables,var)]
    }
  }
  
  variables_missing=as.character(lapply(variables,function(x)paste(x," was not available",sep="")))
  
  variables_eco=unlist(list(variables_available,variables_missing))
  
  ############ 5. CALCULATE Ecocast for preddate 
  print(paste("Running Ecocast: calculating ecocast risk for ",preddate,sep=""))
  ecocastrisk<-ecocastrisk
  # projection(mns[[i]])<-oldproj
  # projection(ses[[i]])<-oldproj
  
  ecocast_m_r<-overlay(get(mns[1])[[1]],get(mns[2])[[1]],get(mns[3])[[1]],get(mns[4])[[1]],get(mns[5])[[1]],fun=EcoCalc) # !!!! use this one
  ecocast_se_r<-overlay(get(ses[1])[[1]],get(ses[2])[[1]],get(ses[3])[[1]],get(ses[4])[[1]],get(ses[5])[[1]],fun=EcoCalc)
  
  PlotEcoCast(ecocast_m_r,preddate,paste(ecocastdir,"mean/",sensitivitydir,sep=''),rescal=TRUE,version="_mean")
  PlotEcoCast(ecocast_se_r,preddate,paste(ecocastdir,"se/",sensitivitydir,sep=''),rescal=FALSE,version="_se")

}
 
