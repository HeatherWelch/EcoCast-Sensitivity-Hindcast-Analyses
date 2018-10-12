## script to run and plot ecocast from predCIs for all species

Run_ecocast=function(get_date,moddir,griddir,outdir,ecocastdir,namesrisk,ecocastrisk,bycatchrisk,final_path_list,logodir,studyarea,staticdir){

  ############ 1. load required functions
  
  ## A. rasterRescale
  rasterRescale<-function(r){
    r.min = cellStats(r, "min")
    r.max = cellStats(r, "max")
    r.scale <- ((r - r.min) / (r.max - r.min) - 0.5 ) * 2
    return(r.scale) #(r-rmin)/(rmax-rmin)
  }
  
  
  ## A2. rasterRescale (-1 to r.max) ## this is for when swordfish = 0, we still rescale the min value to -1 to fit within app.R color range ##test
  #http://stackoverflow.com/questions/12959371/how-to-scale-numbers-values
  alt_rasterRescale=function(r){
    r.min = cellStats(r, "min")
    r.max = cellStats(r, "max")
    r.scale <--1+(r.max--1)*(r-r.min)/(r.max-r.min)
    return(r.scale)
  }
  
  ## B. EcoCast_readraster
  EcoCast_readraster<-function(CIobj,yr="2012",griddir,calctype="m"){
    CIdir<-unlist(strsplit(CIobj,"_"))[2]
    
    assign(paste(CIdir,"dir",sep=""),paste(griddir,CIdir,"/predCIs/",sep=''))
    varname<-paste(CIdir,"dir",sep="")
    
    allfiles<-list.files(get(varname), glob2rx('*.grd'), full.names=T)
    if (calctype=="m") {
      assign(paste("files",yr,"m",sep=''),allfiles[grep(yr,allfiles)][grep("_mean",allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_m_r",sep=''),lapply(get(paste("files",yr,"m",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_m_r",sep='')))
    }
    if (calctype=="se") {
      assign(paste("files",yr,"se",sep=''),allfiles[grep(yr,allfiles)][grep("_se",allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_se_r",sep=''),lapply(get(paste("files",yr,"se",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_se_r",sep='')))
    }
    if (calctype=="highCI") {
      assign(paste("files",yr,"hiCI",sep=''),allfiles[grep(yr,allfiles)][grep("_highCI",allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_hiCI_r",sep=''),lapply(get(paste("files",yr,"hiCI",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_hiCI_r",sep='')))
    }
    if (calctype=="lowCI") {
      assign(paste("files",yr,"loCI",sep=''),allfiles[grep(yr,allfiles)][grep("_lowCI",allfiles[grep(yr,allfiles)])])
      assign(paste(CIdir[i],yr,"_loCI_r",sep=''),lapply(get(paste("files",yr,"loCI",sep='')), FUN = raster))
      return(get(paste(CIdir[i],yr,"_loCI_r",sep='')))
    }
  }
  
  ## C. EcoCalc
  EcoCalc<-function(a,b,c,d,e,risk=risk,clipTarget=TRUE){
    ecorisk<-a*risk[1]+b*risk[2]+c*risk[3]+d*risk[4]+e*risk[5]
    if (clipTarget) {
      (ecorisk[(e<0.25)&(ecorisk>0.5)]=100)
    }
    return(ecorisk)
  }
  
  ## D. EcoCols
  EcoCols<-colorRampPalette(c("red","orange","white","cyan","blue"))
  ByCols<-colorRampPalette(c("red","orange","white"))
  SeCols<-colorRampPalette(c("coral3","cadetblue3","white","cadetblue3","coral3"))
  
  ## E. PlotEcoCast
  PlotEcoCast(ecocast_m_r,get_date,paste(ecocastdir,"mean/",sep=''),rescal=TRUE,risk=risk,version="_mean",type="ecocast") ## standard directory
  
  PlotEcoCast<-function(r,get_date,wd=getwd(),leg=TRUE,scalbar=FALSE,rescal=FALSE,risk=risk,spp=namesrisk,version="_V1",contourval=NA,addLCA=FALSE,addtext=TRUE,type="ecocast"){
    
    ####### produce png ####
    png(paste(wd,"/EcoCast_",paste(risk,collapse="_"),'_',get_date,version,'.png',sep=''),width=9,height=11,units='in',res=300,pointsize = 14)
    par(mar=c(3,3,.5,.5),las=1,font=2)
   
    if (rescal){
      r<-rasterRescale(r)
    } 
    if (rescal==F && version=="_mean"){
      r=alt_rasterRescale(r)
    }
    
    if (version=="_se") {
      zlimits<-c(-0.1,0.1)
      col=SeCols(255)}
    
    if(type=="ecocast" && version=="_mean") {
      zlimits=c(-1,1)
      col=EcoCols(255)}
    
    if(type=="bycast" && version=="_mean") {
      zlimits=c(-1,0)
      col=ByCols(255)}
    
    if (leg) {
      image.plot(r,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits)
    } else {
      image(r,col=col,xlim=c(-130,-115),ylim=c(30,47),zlim=zlimits) ## PRESABS
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
    
    
    
    maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
    if (addtext) {
      text(-122,46,format(get_date,format="%b %d %Y"),adj=c(0,0),cex=2) 
      text(-122,45,"Species weightings",adj=c(0,0),cex=1)
      #text(-122,45,paste(namesrisk[1],' weighting = ',risk[1],sep=''),adj=c(0,0),cex=.75)
      text(-122,44.5,paste(namesrisk[2],' weighting = ',risk[2],sep=''),adj=c(0,0),cex=.75)
      text(-122,44,paste(namesrisk[3],' weighting = ',risk[3],sep=''),adj=c(0,0),cex=.75)
      text(-122,43.5,paste(namesrisk[4],' weighting = ',risk[4],sep=''),adj=c(0,0),cex=.75)
      text(-122,43,paste(namesrisk[5],' weighting = ',risk[5],sep=''),adj=c(0,0),cex=.75)
      
      text(-122,42.5,"Environmental data",adj=c(0,0),cex=1)
      text(-122,42,variables_eco[1],adj=c(0,0),cex=.75)
      text(-122,41.5,variables_eco[2],adj=c(0,0),cex=.75)
      text(-122,41,variables_eco[3],adj=c(0,0),cex=.75)
      text(-122,40.5,variables_eco[4],adj=c(0,0),cex=.75)
      text(-122,40,variables_eco[5],adj=c(0,0),cex=.75)
      text(-122,39.5,variables_eco[6],adj=c(0,0),cex=.75)
      
    }
    
    box()
    dev.off()
    #####
    ####### produce raster ####
    writeRaster(r,filename=paste(wd,'/EcoCast_',paste(risk,collapse="_"),"_",get_date,version,'.grd',sep=''),overwrite=TRUE) 
    
  
    }
  
  ## F. Create latest files
  latest_fucn=function(metric,riskname,risk,ecocastdir,get_date){
    for(file in list.files(paste0(ecocastdir,metric,"/latest"),pattern = paste(risk,collapse="_"))){
      a=gsub(paste(risk,collapse="_"),paste0(riskname,"_latest"),file)
      b=gsub(get_date,"",a)
      c=gsub("__","_",b)
      from=paste0(ecocastdir,metric,"/latest/",file)
      to=paste0(ecocastdir,metric,"/latest/",c)
      file.copy(from=from,to=to,overwrite = T)
    }
  }
  
  
  ############ 4. Make list of variable dates ####
  
  ############ 2. Load species confidence interval grids ####
  CIobjs<-list.files(moddir, glob2rx('*.rds'), full.names=F)
  CIdir<-CIobjs

  for (i in 1:length(CIobjs)) {
    CIdir[i]<-unlist(strsplit(CIobjs[[i]],"_"))[2]
    print(paste("Reading in confidence interval grids for ",CIdir[i],sep=""))
    assign(paste(CIdir[i],get_date,"_m_r",sep=''),EcoCast_readraster(CIobjs[i],yr=get_date,outdir,calctype="m"))
    assign(paste(CIdir[i],get_date,"_se_r",sep=''),EcoCast_readraster(CIobjs[i],yr=get_date,outdir,calctype="se"))
  }
  
  mns<-ls()[grep(paste(get_date,"_m_r",sep=""),ls())]
  ses<-ls()[grep(paste(get_date,"_se_r",sep=""),ls())]

  ############ 3. Define coordinate systems
  projstring <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
  oldproj<-CRS("+proj=longlat +datum=WGS84")
  
  ## first for available variables
  nm=lapply(final_path_list$FileList_final,function(x)unlist(strsplit(x,"/")))
  nm0=lapply(nm,function(x)paste(x[length(x)],x[length(x)-1]))
  variables=as.character(unlist(lapply(nm0,function(x)gsub(".grd","",x[[1]]))))
  variables=gsub("ywind","Surface wind",variables)
  variables=gsub("analysed_sst","Sea surface temperature",variables)
  variables=gsub("l.eke_mean","Eddy kinetic energy",variables)
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
  
  ############ 4. Define global objects
  studyarea=readOGR(dsn=staticdir,layer="sa_square_coast3")
  
  ############ 5. CALCULATE Ecocast for get_date 
  print(paste("Running Ecocast: calculating ecocast risk for ",get_date,sep=""))
  risk<-ecocastrisk
  # projection(mns[[i]])<-oldproj
  # projection(ses[[i]])<-oldproj
  
  #ecocast_m_r<-overlay(get(mns[1])[[1]],get(mns[2])[[1]],get(mns[3])[[1]],get(mns[4])[[1]],get(mns[5])[[1]],fun=EcoCalc) # !!!! use this one
  #ecocast_se_r<-overlay(get(ses[1])[[1]],get(ses[2])[[1]],get(ses[3])[[1]],get(ses[4])[[1]],get(ses[5])[[1]],fun=EcoCalc)
  
  ecocast_m_r=EcoCalc(get(mns[1])[[1]],get(mns[2])[[1]],get(mns[3])[[1]],get(mns[4])[[1]],get(mns[5])[[1]],risk=risk)%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  ecocast_se_r=EcoCalc(get(ses[1])[[1]],get(ses[2])[[1]],get(ses[3])[[1]],get(ses[4])[[1]],get(ses[5])[[1]],risk=risk)%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  
  PlotEcoCast(ecocast_m_r,get_date,paste(ecocastdir,"mean/",sep=''),rescal=TRUE,risk=risk,version="_mean",type="ecocast") ## standard directory
  PlotEcoCast(ecocast_se_r,get_date,paste(ecocastdir,"se/",sep=''),rescal=FALSE,risk=risk,version="_se",type="ecocast") ## standard directory

  PlotEcoCast(ecocast_m_r,get_date,paste(ecocastdir,"mean/latest/",sep=''),rescal=TRUE,risk=risk,version="_mean",type="ecocast") ## latest directory
  PlotEcoCast(ecocast_se_r,get_date,paste(ecocastdir,"se/latest",sep=''),rescal=FALSE,risk=risk,version="_se",type="ecocast") ## latest directory
  
  latest_fucn(metric = "mean",riskname="ecocastrisk",risk,ecocastdir = ecocastdir,get_date = get_date)
  latest_fucn(metric = "se",riskname="ecocastrisk",risk,ecocastdir = ecocastdir,get_date = get_date)
  
  ############ 6. CALCULATE bycatch for get_date 
  print(paste("Running Ecocast: calculating bycatch risk for ",get_date,sep=""))
  risk<-bycatchrisk
  # projection(mns[[i]])<-oldproj
  # projection(ses[[i]])<-oldproj
    
  #bycatch_m_r<-overlay(get(mns[1])[[1]],get(mns[2])[[1]],get(mns[3])[[1]],get(mns[4])[[1]],get(mns[5])[[1]],fun=EcoCalc) # !!!! use this one
  #bycatch_se_r<-overlay(get(ses[1])[[1]],get(ses[2])[[1]],get(ses[3])[[1]],get(ses[4])[[1]],get(ses[5])[[1]],fun=EcoCalc)
  
  bycatch_m_r=EcoCalc(get(mns[1])[[1]],get(mns[2])[[1]],get(mns[3])[[1]],get(mns[4])[[1]],get(mns[5])[[1]],risk=risk)%>%mask(.,studyarea) %>% crop(.,extent(studyarea))
  bycatch_se_r=EcoCalc(get(ses[1])[[1]],get(ses[2])[[1]],get(ses[3])[[1]],get(ses[4])[[1]],get(ses[5])[[1]],risk=risk)%>%mask(.,studyarea) %>% crop(.,extent(studyarea))

  PlotEcoCast(bycatch_m_r,get_date,paste(ecocastdir,"mean/",sep=''),rescal=FALSE,risk=risk,version="_mean",type="bycast") ## standard directory
  PlotEcoCast(bycatch_se_r,get_date,paste(ecocastdir,"se/",sep=''),rescal=FALSE,risk=risk,version="_se",type="bycast") ## standard directory
  
  PlotEcoCast(bycatch_m_r,get_date,paste(ecocastdir,"mean/latest/",sep=''),rescal=FALSE,risk=risk,version="_mean",type="bycast") ## latest directory
  PlotEcoCast(bycatch_se_r,get_date,paste(ecocastdir,"se/latest",sep=''),rescal=FALSE,risk=risk,version="_se",type="bycast") ## latest directory
  
  latest_fucn(metric = "mean",riskname="bycatchrisk",risk ,ecocastdir = ecocastdir,get_date = get_date)
  latest_fucn(metric = "se",riskname="bycatchrisk",risk ,ecocastdir = ecocastdir,get_date = get_date)
  
  ############ 7. make website panel for get_date 
  #-------------------> depreciated code
  #PlotPanel(rasterRescale(ecocast_m_r),ecocast_se_r,alt_rasterRescale(bycatch_m_r),bycatch_se_r,get_date,ecocastrisk,bycatchrisk,ecocastdir)
  
  ############ 8. make final product with metadata
  template=image_read(paste0(logodir,"template2.png"))
  ecocast=image_read(paste(ecocastdir,"mean/EcoCast_",paste(ecocastrisk,collapse="_"),'_',get_date,'_mean.png',sep=''))
  c=image_crop(ecocast,"827x1100+17-0") 
  template2=image_scale(template, "970")
  a=image_composite(template2,c,offset = "+15+240")
  b=image_annotate(a,paste0("Image created ",Sys.Date()," by HW. Next projected image date: ",Sys.Date()+1),size=12,gravity = "southeast",location="+130+230",font = "courier")
  image_write(b,path=paste(ecocastdir,"mean/EcoCast_",paste(ecocastrisk,collapse="_"),'_',get_date,'_mean_product.png',sep=''))
  image_write(b,path=paste(ecocastdir,"mean/latest/EcoCast_",paste(ecocastrisk,collapse="_"),'_',get_date,'_mean_product.png',sep=''))
  image_write(b,path=paste(ecocastdir,"mean/latest/EcoCast_ecocastrisk_latest_mean_product.png",sep=''))
  

  ############ 9. make final four pannel product
  eco_mean=image_read(paste(ecocastdir,"mean/EcoCast_",paste(ecocastrisk,collapse="_"),'_',get_date,'_mean.png',sep=''))#%>%image_scale("500")
  by_mean=image_read(paste(ecocastdir,"mean/EcoCast_",paste(bycatchrisk,collapse="_"),'_',get_date,'_mean.png',sep=''))#%>%image_trim()
  eco_se=image_read(paste(ecocastdir,"se/EcoCast_",paste(ecocastrisk,collapse="_"),'_',get_date,'_se.png',sep=''))#%>%image_trim()
  by_se=image_read(paste(ecocastdir,"se/EcoCast_",paste(bycatchrisk,collapse="_"),'_',get_date,'_se.png',sep=''))#%>%image_trim()
  
  eco_logo=image_read(paste0(logodir,"eco_globe.png"))%>%image_scale("300")

  row1=c(eco_mean,eco_se)
  a=image_scale(row1)
  a=image_append(a)
  row2=c(by_mean,by_se)
  b=image_scale(row2)
  b=image_append(b)

  four=image_append(c(a,b),stack = T)
  four=image_composite(four,eco_logo,offset = "+520+510")
  four=image_annotate(four,"Catch-Bycatch",size=25,color="black", boxcolor = "white",location="+480+450")
  four=image_annotate(four,"Catch-Bycatch - prediction error",size=25,color="black", boxcolor = "white",location="+1440+450")
  four=image_annotate(four,"Bycatch only",size=25,color="black", boxcolor = "white",location="+480+1550")
  four=image_annotate(four,"Bycatch only - prediction error",size=25,color="black", boxcolor = "white",location="+1440+1550")
  
  image_write(four,path=paste(ecocastdir,"mean/EcoCast_",paste(ecocastrisk,collapse="_"),'_',get_date,'_panel.png',sep=''))
  image_write(four,path=paste(ecocastdir,"mean/EcoCast_panel.png",sep=''))
  image_write(four,path=paste(ecocastdir,"mean/latest/EcoCast_panel.png",sep=''))

}