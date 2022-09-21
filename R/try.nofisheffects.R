try.nofisheffects <-
function(namesfile=ifelse(vnumber<2,"SS2names.nam","starter.ss2"),vnumber=2,debug.mode=FALSE,is.plot=T){
  
#  com <- command.alias(.Platform$OS.type)
#  com$system(paste2(com$cp," ",namesfile," ",namesfile,"_o"))
#  com$system(paste2(com$cp," ss2.par ss2_old.par"))  
#  on.exit(com$system(paste2(com$cp," ",namesfile," ",namesfile,"_last")))
#  on.exit(com$system(paste2(com$cp," ",namesfile,"_o ",namesfile)),append=TRUE)

  file.copy2(from=namesfile,to=paste2(namesfile,"_o"))
  file.copy2(from="ss2.par",to="ss2_old.par")
  
  on.exit(file.copy2(from=namesfile,to=paste2(namesfile,"_last")))
  on.exit(file.copy2(from=paste2(namesfile,"_o"),to=namesfile),append=TRUE)  

  names.obj <- names.obj.o <- read.table(namesfile,as.is=T)

  # First fun with full catch
  if(debug.mode==FALSE){
    if(vnumber<3){
      doss2()
    }
    else{
      doss3()
    }
  }
#  com$system(paste2(com$cp," ss2.rep ss2_org.rep"))
  if(vnumber<3){
    file.copy2(from="ss2.rep",to="ss2_org.rep")
  }
  else{
    file.copy2(from="report.sso",to="ss2_org.rep")    
  }

  a <- replacedat.catch(names.obj[1,1],outfile="tmp.dat",zero.fleets=NULL)
  nfish <- as.numeric(a$nfish)
  outname <- paste2("minusF1to",1:nfish)
  
  for(i in 1:nfish){
    browser()
    a <- replacedat.catch(names.obj[1,1],outfile=paste2(outname[i],".dat"),zero.fleets=1:i)      
    names.obj[1,1] <- paste2(outname[i],".dat")
    write.table("# Nustarter.SS2 \n",file=namesfile,row.names=F,col.names=F,quote=FALSE)
    write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)
    if(debug.mode==FALSE){
      if(vnumber<3){
        doss2()
      }
      else{
        doss3()
      }
      }
#    com$system(paste2(com$cp," ss2.rep ",outname[i],".rep"))
#    com$system(paste2(com$cp," ss2_old.par ss2.par"))
    if(vnumber<3){
      file.copy2(from="ss2.rep",to=paste2(outname[i],".rep"))
    }
    else{
      file.copy2(from="report.sso",to=paste2(outname[i],".rep"))      
    }
    file.copy2(from="ss2_old.par",to="ss2.par")          
  }

  biom <- as.list(1:(nfish+1))
 # getBabs
  for(i in 1:nfish){
    biom[[i]]  <- getBabs.ss2(paste2(outname[i],".rep"))[[1]]
  }
  biom[[nfish+1]] <- getBabs.ss2("ss2_org.rep")[[1]]

  biom.mat <- matrix(0,nrow(biom[[1]]),nfish+1)
  qt <- max(biom[[1]]$season)
  dimnames(biom.mat) <- list(biom[[1]]$year+biom[[1]]$season/qt-1/qt,c(outname,"Org"))
  for(i in 1:nfish){
    biom.mat[,i] <- biom[[i]]$SpawnBio
  }
  biom.mat[,nfish+1] <- biom[[nfish+1]]$SpawnBio  
#  plotBSR(biom[[nfish]],biom[c(1:(nfish-1),nfish+1)])

  if(is.plot==TRUE){
    plot.nofishBio(biom.mat,datfile=names.obj[1,1])
  }

  invisible(list(biom.list=biom,biom.mat=biom.mat))
}
