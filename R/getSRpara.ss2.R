getSRpara.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL,qt=4){
  if(is.ss3(repfile)){
    read.char <- "PARAMETERS"
    line.tmp <- 2
    gyou.tmp <- NULL
  }
  else{
    read.char <- "SR_parms"
    line.tmp <- 0
    gyou.tmp <- 6
  }
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    SRpara <- find.and.read.table2(read.char,skipline=0+line.tmp,gyou=gyou.tmp,fill=T,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    SRpara <- find.and.read.table(read.char,skipline=0+line.tmp,startpoint=target.line,gyou=gyou.tmp,
                               table.property=cl,comment.char="",fill=T,
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  tmp <- as.data.frame(SRpara[[1]])
#  if(is.ss3(repfile)){
#    tmp <- tmp[,-1]
#  }  
  biom <- getBabs.ss2(repfile,cl=cl,tb=tb)
  if(is.ss3(repfile)){ # for version of > 3.00
    tmp2 <- substr(tmp[,2],1,12)=="Main_RecrDev" & tmp[,14]=="act"
    Rdev.seed <- tmp[tmp2,3]
    names(Rdev.seed) <- substr(tmp[,2],14,17)[tmp2]
    SRpara[[1]] <- list(logR0=tmp[tmp[,2]=="SR_R0",3],
                        steepness=tmp[tmp[,2]=="SR_steep",3],
                        sigmaR=tmp[tmp[,2]=="SR_sigmaR",3],
                        envlink=tmp[tmp[,2]=="SR_envlink",3],
                        logR1=tmp[tmp[,2]=="SR_R1_offset",3],
                        Future=tmp[tmp[,2]=="SR_autocorr",3],  #???
                        SSB0=biom[[1]]$SpawnBio[!is.na(biom[[1]]$SpawnBio)&biom[[1]]$period=="VIRG"],
                        Rdev.seed=Rdev.seed)
  }
  else{  # for version older than 3.00
    SRpara[[1]] <- list(logR0=tmp[1,2],steepness=tmp[2,2],sigmaR=tmp[3,2],
                        envlink=tmp[4,2],logR1=tmp[5,2],Future=tmp[6,2],
                        SSB0=biom[[1]]$SpawnBio[1])    
  }
  SRpara[[3]] <- tmp
  SRpara
}
