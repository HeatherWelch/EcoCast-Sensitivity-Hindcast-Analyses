#### deleting Sensitivty files

  outdir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns"
  
  dir=list.dirs(outdir,full.names=TRUE)
  
  for(d in dir){
    L=list(list.files(d,pattern = "EcoCast",full.names = TRUE))
    do.call(file.remove,L)
    
  }
  
  for(d in dir){
    L=list(list.files(d,pattern = "pa",full.names = TRUE))
    do.call(file.remove,L)
    
  }