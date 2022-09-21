plotSRcurve <-
function(repfile=NULL,SRfunc=NULL,parameter=NULL,add=FALSE,col.var=1,plotVB=FALSE,biomass.unit="",rec.unit=""){
  if(is.null(SRfunc)&!is.null(repfile)){
    SRfunc <- getSRfunc.ss2(repfile)[[1]]
  }
  else{
    if(is.null(repfile)) cat("please enter file name of repfile or biomass object!!")
  }
  if(is.null(parameter)&!is.null(repfile)){
    parameter <- getSRpara.ss2(repfile)[[1]]
  }
  else{
    if(is.null(repfile)) cat("please enter file name of repfile or parameter object!!")
  }

  if(!plotVB){
    x.tmp <- SRfunc$year!="Virg"
    }
  else{
    x.tmp <- TRUE
  }

  if(sum(colnames(SRfunc)=="era")>0){
    pch.tmp <- as.numeric(SRfunc$era)
  }
  else{
    pch.tmp <- 1
  }
    #---------- plot
  if(add==FALSE){
    plot(x <- SRfunc$"spawn_bio"[x.tmp],y <- SRfunc$"pred_recr"[x.tmp],
         xlab=paste2("SSB",biomass.unit),ylab=paste("Recruits",rec.unit),
         ylim=c(0,max(y,na.rm=T)),
         xlim=c(0,max(x,na.rm=T)),pch=pch.tmp[x.tmp])
    title("Spawner&Recruitment",line=1)
  }
  else{
    points(SRfunc$"spawn_bio"[x.tmp],SRfunc$"pred_recr"[x.tmp],col=col.var,
           pch=pch.tmp)    
  }
  points(x <- seq(from=0,to=max(SRfunc$"spawn_bio"[x.tmp],na.rm=T),length=100),
         y0 <- Beverton.holt.ss2(SSB=x,coef=c(parameter$steepness,exp(parameter$logR0),
                                   B0=SRfunc$SPBzero[1])),xlim=c(0,max(x)),
                           type="l",lwd=2,col="chartreuse3")

  text(rev(x)[1],rev(y0)[1],"R0")
  if(sum(colnames(SRfunc)=="era")>0 & add==FALSE){
    legend("topright",legend=unique(SRfunc$era[x.tmp]),pch=as.numeric(unique(SRfunc$era[x.tmp])))
  }  
  invisible(list(SRfunc=SRfunc,parameter=parameter))
}
