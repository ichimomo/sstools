plotBSR <-
function(biom,biom.list=NULL,true.value=NULL,what.plot=c(T,T,T),
                    col.var=rep(1,1+length(biom.list)),lty.var=rep(1,1+length(biom.list)),
                    lwd.var=rep(1,1+length(biom.list)),ptype=c("l","b"),nline=-1.5,vnumber=2,
                    rev.setting=FALSE,ylim.factor=1,catch.plot=F,add=FALSE,x.adjust=0.25,
                    rec.unit="(1000 fish)",biomass.unit="(tons)",
                    ...){

  if(rev.setting==FALSE){
    par(mfrow=c(sum(what.plot),1),mar=c(4,4,2,0),oma=c(1,1,3,3),mgp=c(2,1,0))
  }
  islist <- is.list(biom.list)
  ylim.res <- rep(0,3)

  ## Total biomass
  tmp1 <- biom$period=="TIME"
  tmp2 <- biom$period=="FORE"
  tmp <- tmp1|tmp2
  vnumber <- 2
  if(sum(tmp)==0){ # for the version older than 2.00
    tmp <- biom$season!="E"
    vnumber <- 1
  }

  biom.summary <- matrix(0,sum(tmp),ifelse(islist,length(biom.list)+1,1))
  rownames(biom.summary) <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust
  if(islist){
    len.list <- length(biom.list)
  }
  
  if(what.plot[1]==T){
    x <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust
    y <- biom.summary[,1] <- biom$"bio-all"[tmp]
    if(add==FALSE){
      plot(tmp <- x[y!=0],y[y!=0],ylab=paste("Total biomass",biomass.unit,sep=""),ylim=c(0,ylim.res[1] <- max(y,na.rm=T)*ylim.factor),
           xlab="Years",type=ptype[1],lwd=lwd.var[1],...)
    }
    else{
      points(tmp <- x[y!=0],y[y!=0],ylab=paste("Total biomass",biomass.unit,sep=""),ylim=c(0,ylim.res[1] <- max(y,na.rm=T)*ylim.factor),
           xlab="Years",type=ptype[1],lwd=lwd.var[1],...)      
    }
    axis(at=floor(min(tmp*0.95)):floor(max(tmp*1.05)),labels=NA,side=1)
         
    if(vnumber>=2 & sum(tmp2)>0)
      points(biom$year[tmp2]+as.numeric(biom$season[tmp2])/4-x.adjust,biom$"bio-all"[tmp2],
             type=ptype[2],pch=3,cex=0.5)
    title("Total biomass",line=nline)
    
    if(islist){
      old.tmp <- tmp
      for(i in 1:len.list){
        if(vnumber<2){
          tmp <- biom.list[[i]]$season!="E"
        }
        else{
          tmp1 <- biom.list[[i]]$period=="TIME"
          tmp2 <- biom.list[[i]]$period=="FORE"
          tmp <- tmp1|tmp2    
        }
        x1 <- xl2 <- biom.list[[i]]$year[tmp]+as.numeric(biom.list[[i]]$season[tmp])/4-x.adjust
        xx.tmp <- match(xl2,rownames(biom.summary))
#        y1 <- biom.summary[remove.nadata(xx.tmp),i+1] <- biom.list[[i]]$"bio-all"[tmp&!is.na(xx.tmp)]
        y1 <- biom.summary[xx.tmp,i+1] <- biom.list[[i]]$"bio-all"[tmp]        
        points(x1[y1!=0],y1[y1!=0],col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[1])
        if(vnumber>=2 & sum(tmp2)>0)
          points(biom.list[[i]]$year[tmp2]+as.numeric(biom.list[[i]]$season[tmp2],
                                                      biom.list[[i]]$"bio-all"[tmp2])/4-x.adjust,pch=3,
                 col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[2],cex=0.5)
      }}

  # TRUE abundance in using the result of Operation Model
    if(!is.null(true.value)){
#      a <- readOM2(true.value,fish.year=0.5)
      points(true.value$year,true.value$biomass,col="plum1",lwd=2,type="l")    
    }
  }

  ## Spawning biomass
  if(vnumber<2){
    tmp <- biom$season!="E"
  }
  else{
    tmp1 <- biom$period=="TIME";tmp2 <- biom$period=="FORE"
    tmp <- tmp1|tmp2    
  }

  x.adjust <- 1
  ssb.summary <- matrix(0,sum(tmp),ifelse(islist,length(biom.list)+1,1))
  rownames(ssb.summary) <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust

  if(what.plot[2]==T){
#    x <- biom$year[tmp]+as.numeric(biom$season[tmp])/4-x.adjust
    x <- biom$year[tmp]
    y <- ssb.summary[,1] <- biom$"SpawnBio"[tmp]
    if(add==FALSE){
      plot(tmp <- x[y!=0&!is.na(y)],y[y!=0&!is.na(y)],ylab=paste("SSB",biomass.unit,sep=""),xlab="Years",type=ptype[1],lwd=lwd.var[1],ylim=c(0,ylim.res[2] <- max(y,na.rm=T)*ylim.factor),
           ...)
    }
    else{
      points(tmp <- x[y!=0&!is.na(y)],y[y!=0&!is.na(y)],ylab=paste("SSB",biomass.unit,sep=""),xlab="Years",type=ptype[1],lwd=lwd.var[1],ylim=c(0,ylim.res[2] <- max(y,na.rm=T)*ylim.factor),
           ...)      
    }
    axis(at=floor(min(tmp*0.95)):floor(max(tmp*1.05)),labels=NA,side=1)    
    title("SSB",line=nline)

    if(vnumber>=2 & sum(tmp2)>0)
#      points(biom$year[tmp2]+as.numeric(biom$season[tmp2])/4-x.adjust,biom$"SpawnBio"[tmp2],
      points(biom$year[tmp2],biom$"SpawnBio"[tmp2],      
             type=ptype[1],pch=3,cex=0.5)
  
    if(islist){
      for(i in 1:len.list){
        if(vnumber<2){
          tmp <- biom.list[[i]]$season!="E"
        }
        else{
          tmp1 <- biom.list[[i]]$period=="TIME";tmp2 <- biom.list[[i]]$period=="FORE"
          tmp <- tmp1|tmp2    
        }
     
#        xl <- biom.list[[i]]$year[tmp]+as.numeric(biom.list[[i]]$season[tmp])/4-x.adjust
        xl <- biom.list[[i]]$year[tmp]
        xl2 <- biom.list[[i]]$year[tmp]+as.numeric(biom.list[[i]]$season[tmp])/4-x.adjust
#        yl <- ssb.summary[,i+1] <- biom.list[[i]]$"SpawnBio"[tmp]
        xx.tmp <- match(xl2,rownames(ssb.summary))        
        yl <- ssb.summary[xx.tmp,i+1] <-
                                    biom.list[[i]]$"SpawnBio"[tmp]
        points(xl[yl!=0 & !is.na(yl)],yl[yl!=0 & !is.na(yl)],
               col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[1])
        if(vnumber>=2 & sum(tmp2)>0)
          points(biom.list[[i]]$year[tmp2]+as.numeric(biom.list[[i]]$season[tmp2])/4-x.adjust,
                 biom.list[[i]]$"SpawnBio"[tmp2],pch=3,
                 col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[1],cex=0.5)      
      }}

    #!!!! this algorism should be changed if the recruitment is occured at other than 1st quarter
    if(vnumber<2){
      tmp <- biom$season!="E"
    }
    else{
      tmp1 <- biom$period=="TIME";tmp2 <- biom$period=="FORE"
      tmp <- tmp1|tmp2    
    }
    if(!is.null(true.value)){
#      a <- readOM2(true.value,fish.year=0.5)
      points(true.value$year,true.value$ssb,col="plum1",lwd=2,type="l")    
    }    
  }

  tmp3 <- tmp&biom$"recruit-0">0
#  recruit.summary <- matrix(0,sum(tmp3),ifelse(islist,length(biom.list)+1,1))
#  rownames(recruit.summary) <- biom$year[tmp3]+as.numeric(biom$season[tmp3])/4-x.adjust
  
  x.adjust <- 0.25
  if(what.plot[3]==T){
    x <- biom$year[tmp3]+
                   as.numeric(biom$season[tmp3])/4-x.adjust
#    y <- recruit.summary[,1] <- biom$"recruit-0"[tmp3]
    y <- biom$"recruit-0"[tmp3]        

    ylim.res[3] <- max(y,na.rm=T)*ylim.factor
    if(add==FALSE){
      plot(tmp <- x[y!=0],y[y!=0],ylab=paste("Recruits",rec.unit,sep=""),xlab="Years",type=ptype[1],lwd=lwd.var[1],ylim=c(0,ylim.res[3]),
           ...)
    }
    else{
      points(tmp <- x[y!=0],y[y!=0],ylab=paste("Recruits",rec.unit,sep=""),xlab="Years",type=ptype[1],lwd=lwd.var[1],ylim=c(0,ylim.res[3]),
           ...)      
    }
    axis(at=floor(min(tmp*0.95)):floor(max(tmp*1.05)),labels=NA,side=1)    
    title("Number of recruits",line=nline)
    if(vnumber>=2 & sum(tmp2)>0){
      tmp3 <- tmp2&biom$"recruit-0">0
      points(biom$year[tmp3]+as.numeric(biom$season[tmp3])/4-x.adjust,biom$"recruit-0"[tmp3],
             type=ptype[2],pch=3,cex=0.5)
    }
    if(islist){
      for(i in 1:len.list){
        if(vnumber<2){
          tmp <- biom.list[[i]]$season!="E"
        }
        else{
          tmp1 <- biom.list[[i]]$period=="TIME";tmp2 <- biom.list[[i]]$period=="FORE"
          tmp <- tmp1|tmp2    
        }
      
        tmp3 <- tmp&biom.list[[i]]$"recruit-0">0
        xl <- xl2 <- biom.list[[i]]$year[tmp3]+
          as.numeric(biom.list[[i]]$season[tmp3])/4-x.adjust
        xx.tmp <- match(xl2,rownames(ssb.summary))
#        yl <- recruit.summary[xx.tmp,i+1] <- biom.list[[i]]$"recruit-0"[tmp3]
        yl <- biom.list[[i]]$"recruit-0"[tmp3]        
        points(xl[yl!=0],yl[yl!=0],col=col.var[i+1],lty=lty.var[i+1],
               lwd=lwd.var[i+1],type=ptype[1])
        if(vnumber>=2 & sum(tmp2)>0){
          tmp3 <- tmp2&biom$"recruit-0">0          
          points(biom.list[[i]]$year[tmp3]+as.numeric(biom.list[[i]]$season[tmp3])/4-x.adjust,
                 biom.list[[i]]$"recruit-0"[tmp3],pch=3,
                 col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type=ptype[2],cex=0.5)            
      }}}

    if(!is.null(true.value)){
#      a <- readOM2(true.value,fish.year=0.5)
#      points(a$recruit$x,a$recruit$y/1000,col="plum1",lwd=3,type="l")
      points(true.value$year,true.value$recruit,col="plum1",lwd=2,type="l")          
    }
  }

  invisible(list(biom=biom,biom.list=biom.list,biom.summary=biom.summary,
                 ssb.summary=ssb.summary,#recruit.summary=recruit.summary,
                 ylim.res=ylim.res))
}
