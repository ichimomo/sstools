allplot.ss2 <-
function(repfile="Report.sso",outfile="out",is.plot.cohortSSB=FALSE,
                                      dev.type="pdf",len.residual.plot=TRUE,
                                      length.on=TRUE,aselect.on=F,select.on=TRUE,
                                      col.var=NULL,lty.var=NULL,
                                      lwd.var=NULL,true.value=NULL,
                                      repfile.legend=NULL,compfile="CompReport.sso",
                                      multilambda=TRUE,
                                      biomass.unit=" (tons)",rec.unit=" (1000 fish)",length.unit=" (cm)",
                                      refpoint.year=NULL,  # If you want to calculate reference point, input years of reference F for selectivivty, such as 2004:2006.  This option needs optional functions distributed by Yukio Takeuchi (yukiot@fra.affrc.go.jp).  
                                      datfile=NULL#,plot.resid=F
                                      ){

  #!!!! Known bugs !!!!!
  # *** Not work if 1st option of F method (using Pope's approximation)
  #        * please tentatively replace 'F:' in my code with 'Hrate:' or 'Hrate:' with 'F:' in report file.  
  
  nline <- 0.5

  #-------------  error check if specified files exist ---
  tmp <- which(file.info(repfile)$size==0 | is.na(file.info(repfile)$size))
  if(length(tmp)>0){
    stop(message=paste("ERROR:: no file or no data in Repfile exits for ", repfile[tmp],".\n",sep=""))
  }

  if(length.on==TRUE){
    if(is.null(compfile) && is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      stop(message="ERROR:: Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"! for plotting length or age composition data with option of length.on=T")
    }
    if(is.ss3(repfile)){
      tmp <- which(file.info(compfile)$size==0 | is.na(file.info(compfile)$size))
      if(length(tmp)>0){
        stop(message=paste("ERROR:: no file or no data in CompReport exits for ", repfile[tmp],".\n",sep=""))
      }}
  }
  #------------- error check done

  if(is.list(repfile)){
    repfile <- unlist(repfile)
  }

  #------------ SET for ubnormal termination 
  exitfunc <- function(){
    if(names(dev.cur())!="null device") dev.off()
#    par(org.par)
  }
  on.exit(exitfunc())

  #------------ SET graphic parameters for multiplot
  if(is.null(lty.var)){
    if(length(repfile)>1) lty.var <- rep(1,length(repfile))
    else lty.var <- 1
  }
  if(is.null(col.var)){
    if(length(repfile)>1){
      col.var <- #c(1,good.colors())
        c(1:4,"#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3",
          "#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D",
          good.colors(),1:length(repfile))
    }
    else{
      col.var <- 1
    }
  }
  
  if(is.null(lwd.var)){
    if(length(repfile)>1) lwd.var <- rep(1,length(repfile))
    else lwd.var <- 1
  }

  if(!is.null(datfile)){
    fleet.name <- read.fleetname(datfile)
  }
  else{
    fleet.name <- NULL
  }

  if(dev.type=="ps") postscript(paste(outfile,".ps",sep=""),horizontal=FALSE,height=9,width=9)
  if(dev.type=="pdf") pdf(paste(outfile,".pdf",sep=""),height=11,width=9)#,paper="a4")
  if(dev.type=="x11") x11()

  #---------- SET graphical parameters
  org.par <- par()
  par(mgp=c(2,0.3,0),tck=0.03,font.main=1,cex.main=1,ps=14,oma=c(1,1,3,3),mar=c(4,6,3,0))
#   par(mfrow=c(3,1),mar=c(4,6,3,0),oma=c(0,0,3,3))  
  multiplot <- length(repfile)>1

  #---------- plot information
  par(ps=12,oma=c(0,0,0,0),mar=c(2,2,2,2))
  plot.info(repfile)
  par(ps=14,oma=c(1,1,3,3),mar=c(4,6,3,0))  

  #---------- READ report files
  cl0 <- count.fields(repfile[1],blank.lines.skip=FALSE)
  tb0 <- read.table(repfile[1],fill=TRUE,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                    blank.lines.skip=FALSE,colClasses=rep("character",max(cl0)))

  tmp <- getBabs.ss2(repfile[1],cl=cl0,tb=tb0)
  biom <- list(tmp[[1]])
  
  tmp <- getSPR.ss(repfile[1],cl=cl0,tb=tb0)
  SPR <- list(tmp[[1]])

  tmp <- getCPUE.ss2(repfile[1],cl=cl0,tb=tb0)
  cpue <- list(tmp[[1]])

  tmp <- getNAA.ss2(repfile[1],cl=cl0,tb=tb0)
  naa <- list(tmp[[1]])

  tmp <- getCAA.ss2(repfile[1],cl=cl0,tb=tb0)
  caa <- as.list(numeric())
  caa[[1]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])

  tmp <- getNMA.ss2(repfile[1],cl=cl0,tb=tb0)
  nma <- list(tmp[[1]])

  alk <- list()
  alk[[1]] <- getALK.ss(repfile[1],cl=cl0,tb=tb0,all=TRUE)[[1]]

  tmp <- getSRfunc.ss2(repfile[1],cl=cl0,tb=tb0)
  SRfunc <- list(tmp[[1]])

  tmp <- getSRpara.ss2(repfile[1],cl=cl0,tb=tb0)
  SRpara <- list(tmp[[1]])  

  len.rep <- length(repfile)
  ptype <- "l"  
  if(multiplot){
    cl.tmp <- tb.tmp <- as.list(numeric())
    for(i in 2:len.rep){
      cl.tmp[[i]] <- count.fields(repfile[i],blank.lines.skip=FALSE)
      tb.tmp[[i]] <- read.table(repfile[i],fill=T,col.names=paste("V",1:max(cl.tmp[[i]]),sep=""),as.is=T,
                        blank.lines.skip=FALSE,colClasses=rep("character",max(cl.tmp[[i]])))[,1:4]
      biom[[i]] <- getBabs.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SPR[[i]] <- getSPR.ss(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      cpue[[i]] <- getCPUE.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      alk[[i]] <- getALK.ss(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]],all=TRUE)[[1]]
      naa[[i]] <- getNAA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      
      tmp <- getCAA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])
      caa[[i]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])
      
      nma[[i]] <- getNMA.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SRfunc[[i]] <- getSRfunc.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SRpara[[i]] <- getSRpara.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]      
      cat("Read",repfile[i],"file\n")


    }
  }
  else{
    tb.tmp <- list(tb0)
    cl.tmp <- list(cl0)    
  }
  #--------------- FINISH reading Report file

  ## Plot biomass, SSB and recruitment ##
  par(mfrow=c(3,1))  
  if(!multiplot){
    plotBSR(biom[[1]],NULL,
            true.value=true.value,what.plot=c(T,T,T),rev.setting=TRUE,
            col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline,
            biomass.unit=biomass.unit,rec.unit=rec.unit)
  }
  else{
    plotBSR(biom[[1]],biom[2:len.rep],
            true.value=true.value,what.plot=c(T,T,T),rev.setting=TRUE,
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline,
            biomass.unit=biomass.unit,rec.unit=rec.unit)    
  }

  if(is.null(repfile.legend)) repfile.legend <- repfile
  legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)

  mtext(side=3,line=0.5,adj=0.1,paste2("@ Biomass, SSB and Recruitment "),outer=T)
  cat("Plot of estimated biomass, SSB and Recruitments was done.\n")

  ## Show reference points (option)
  if(!is.null(refpoint.year)){
    par(mfrow=c(3,1))
    BRPs <-  try(calcBRP(repfile=repfile,year=refpoint.year[1]:refpoint.year[2],Fmed=T,tol=0.00001))
    if(class(BRPs)=="try-error"){
      cat("Sorry Reference points can't be calculated without optional functions of 'calcBRP'
           Skip this process..")      
    }
    else{
      par(xpd=T)
      if(is.matrix(BRPs)){
        b <- barplot(BRPs,col=1:nrow(BRPs),beside=T)
        for(i in 1:nrow(b)){
          text(b[i,],BRPs[i,],round(BRPs[i,],2),pos=4,srt=90)
        }
        nplot()
        legend("topright",fill=1:nrow(BRPs),legend=repfile,ncol=1)        
      }
      else{
        b <- barplot(BRPs)
        text(b,0.05,round(BRPs,2),pos=4,srt=90)
      }
      par(xpd=F)      
      write.csv(BRPs,file=paste2("BRPs",Sys.Date(),".csv"))
    }
  }
  else{
    BRPs <- NULL
  }

  par(mfrow=c(3,1))  
  ## Plot estimated (or fixed) growth curve -------------------------------------
  a <- plotgrowth2.ss(nma,col.var=col.var,list.name=repfile,length.unit=length.unit)

  if(0){ #ALKを使った旧バージョン
    # プロットするときの最大の体長を計算しておく    
    tmpfunc <- function(x) as.numeric(x[x[,1]=="mean",])+as.numeric(x[x[,1]=="sdsize",])
    tmpfunc2 <- function(x) sapply(x[-length(x)],tmpfunc)[-1,]
    a <- sapply(alk,tmpfunc2)

    # プロット
    label.tmp <- alk[[1]][[length(alk[[1]])]]
    morphs <- unique(label.tmp[,4])
    tmp <- which(label.tmp[,2]==1 & label.tmp[,4]==morphs[1])
    #  browser()
    plotgrowth.ss(as.matrix(alk[[1]][[tmp]]),lwd.var=lwd.var[1],
                  col.var=col.var[1],lty.var=lty.var[1],ylim=c(0,max(a)))
    if(length(morphs)>1){
      for(k in 2:length(morphs)){
        tmp <- which(label.tmp[,2]==1 & label.tmp[,4]==morphs[k])
        plotgrowth.ss(as.matrix(alk[[1]][[tmp]]),lwd.var=lwd.var[1],
                      col.var=col.var[1],lty.var=lty.var[1],add=TRUE,pch=k)
      }
      legend("bottomright",pch=1:length(morphs),legend=paste("growth morph",morphs))
    }
  title("Estimated growth curve (1st season in each age)",line=nline)
  if(multiplot){
    for(i in 2:len.rep){
      label.tmp <- alk[[i]][[length(alk[[i]])]]
      morphs <- unique(label.tmp[,4])
      tmp <- which(label.tmp[,2]==1 & label.tmp[,4]==morphs[1])      
      plotgrowth.ss(as.matrix(alk[[i]][[tmp]]),lwd.var=lwd.var[i],col.var=col.var[i],
                    lty.var=lty.var[1],add=TRUE)
      if(length(morphs)>1){
        for(k in 2:length(morphs)){
          tmp <- which(label.tmp[,2]==1 & label.tmp[,4]==morphs[k])          
          plotgrowth.ss(as.matrix(alk[[i]][[tmp]]),lwd.var=lwd.var[i],
                        col.var=col.var[i],lty.var=lty.var[i],add=TRUE,pch=k)
        }      
      }}}
  }
  ## Plot SR curve 
  plotSRcurve(repfile=repfile[1],SRfunc=SRfunc[[1]],parameter=SRpara[[1]],biomass.unit=biomass.unit,rec.unit=rec.unit)
  if(multiplot){
    for(i in 2:len.rep){
      plotSRcurve(repfile=repfile[i],SRfunc=SRfunc[[i]],parameter=SRpara[[i]],
                  add=T,col.var=col.var[i])
    }
  }

  ##----------  Plot natural mortality --------------------
  x <- tapply(nma[[1]]$M,nma[[1]]$"Age_Mid",mean)
  plot(names(x),x,type="l",lwd=2,ylim=c(0,max(x)*1.1),xlab="Age",ylab="Natural mortality (/year)")
  if(multiplot){
    for(i in 2:len.rep){
      x <- tapply(nma[[i]]$M,nma[[i]]$"Age_Mid",mean)
      points(names(x),x,type="l",col=col.var[i],lwd=2)
    }
  }
  title("M at age",line=nline)  
  ##-------------------------------------------------------
  mtext(side=3,line=0.5,adj=0.1,"@ Growth curve, Spawner-recruitment and M ",outer=T)
  cat("Plot of growth curve was done.\n")  
  
  # legend
  if(multiplot){
#    if(is.null(repfile.legend)){
#      legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
#    }
#    else{
    legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
#    }
  }
  #---------------------------------------------------------------------------------
  
  ## Plot SSB by cohorts ##
  if(is.plot.cohortSSB==TRUE){
    par(mfcol=c(4,1))    
    for(i in 1:len.rep){
      plot.cohortSSB(repfile[i],naa=naa[[i]],nma=nma[[i]],title.text=paste("Numbers of spawners by year-class (",repfile[i],")"))
      par(las=1)
      if(i%%4==1) mtext(side=3,line=0.5,adj=0.1,"@ SSB by year-class",outer=T)    
    }
    if(i<4) mtext(side=3,line=0.5,adj=0.1,"@ SSB by year-class",outer=T)    
    cat("Plot of numbers by year class was done.\n")
  }
  
  
  ## Plot F and exploitation rates##
#   par(mfrow=c(3,1),mar=c(4,6,3,0),oma=c(0,0,3,3))

  if(!multiplot){
    plot.data.frame(SPR[[1]],NULL,"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"SPR",title="SPBfished/SPBzero",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
  }
  else{
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"SPR",title="SPBfished/SPBzero",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)    
  }
  mtext(side=3,line=0.5,adj=0.1,"@ Total exploitation rates, SPR and Y/R",outer=T)      

  ## Plot F and exploitation rates (2): calculated from numbers and catch at age ##
  ## Read faa here
  par(mfrow=c(3,1),mar=c(3,3,2,1))  
  faa <- list(calFAA.ss(repfile=NULL,
                         datas=list(naa=naa[[1]],caa=caa[[1]],nma=nma[[1]],biom=biom[[1]]),is.plot=T))
  x <- rowtapply(apply(faa[[1]]$faa.array,c(1,2),sum))
  x <- cbind(x[,1:6],apply(x[,7:ncol(x)],1,mean))
  matplot(rownames(x),x,type="b",ylab="F",xlab="Years",pch=1:ncol(x),col=1)
  legend("topleft",legend=c(0:5,"6+"),ncol=3,col=1,lty=1:ncol(x),title="Ages",
         pch=1:ncol(x))
  mtext(side=3,line=0.5,adj=0.1,
        paste2("@ F at age by year (",repfile[1],") "),outer=T)  
  
  if(multiplot){
    for(i in 2:len.rep){
      faa[[i]] <- calFAA.ss(repfile=NULL,
                             datas=list(naa=naa[[i]],caa=caa[[i]],nma=nma[[i]],biom=biom[[i]]),
                             is.plot=T)
      x <- rowtapply(apply(faa[[i]]$faa.array,c(1,2),sum))      
      x <- cbind(x[,1:6],apply(x[,7:ncol(x)],1,mean))            
      matplot(rownames(x),x,type="b",ylab="F",xlab="Years",
              pch=1:ncol(x),col=1)
      legend("topleft",legend=c(0:5,"6+"),ncol=3,col=1,lty=1:ncol(x),title="Ages",
             pch=1:ncol(x))
      mtext(side=3,line=0.5,adj=0.1,
            paste2("@ F at age by year (",repfile[i],") "),outer=T)
#      mtext(side=3,line=0.5,adj=0.1,"@ F at age by year",outer=T)      
    }
  }

  #----------------- Fishing impacts by year ------------------
  plotFAA.ss(faa,col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
             repfile.legend=repfile.legend)
  plotFAA.ss(faa,col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
             repfile.legend=repfile.legend,by.age=FALSE)  
  cat("Plot of estimated F and exploitation rates was done.\n")
  ##-------------------------------------------------------------------------------

  #----------------- Exploitation rates by fleet (average by season) derived from Report file
  #  This find seq is depending on the option of F method.  
  #  for(i in 1:len.rep){  
  #    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"F:","Hrate")
  #  }
  #if(!multiplot){
  #  plotTotcatch(biom[[1]],NULL,byyear=TRUE,
  #               ylab="",findseq=c("F:","Hrate"),FUN=mean,
  #                 titlename="@ F by fleet (average through season)",
  #               col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
  #               plot.obscatch=F)
  #}
  #else{
  #  plotTotcatch(biom[[1]],biom[2:len.rep],byyear=TRUE,
  #               ylab="",findseq=c("F:","Hrate"),FUN=mean,
  #               titlename="@ F by fleet (average through season)",
  #               col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
  #               plot.obscatch=F)    
  #}
  #cat("Plot of estimated exploitation rates was done.  \n")

  #----------------- Plot total catch in weight##
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(B)","ret_catch")
  }
#  browser()
  c.unit <- tb0[69,-1]
  c.unit <- c.unit[c.unit!=""]
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,
                 findseq=char.tmp,titlename="@ Total catch in weight (gray: observed, others: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=(c.unit=="1"))
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],
                 findseq=char.tmp,titlename="@ Total catch in weight (gray: observed, others: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=(c.unit=="1"))    
  }
  cat("Plot of estimated total catches (in weight) was done.  \n")

  #-------- total catch by number
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(N)","ret_catch")
  }
  if(!multiplot){
    dummy.var <- NULL
  }
  else{
    dummy.var <- biom[2:len.rep]
  }
  plotTotcatch(biom[[1]],dummy.var,#NULL,
               findseq=char.tmp,titlename="@ Total catch in number (gray: observed, others: expected)",
               col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
               plot.obscatch=(c.unit=="2"))
  cat("Plot of estimated total catches (in number) was done.  \n")  


  ### CPUE
  plotCPUE.ss(cpue,col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline)
  cat("Plot of CPUE (obs vs. est) was done.  \n")  

  # Selectivity
  if(select.on==TRUE){
    nfile <- 1
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0)
    selects <- list(tmp[[1]])

    if(multiplot){
      for(i in 2:len.rep){
        selects[[i]] <- getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      }
    }

    plotSelect(selects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of length selectivity was done.  \n")
  }

  # Age Selectivity
  if(aselect.on==TRUE){
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0,len=FALSE)
    aselects <- list(tmp[[1]])
    
    if(multiplot){
      for(i in 2:len.rep){
        aselects[[i]] <- getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]],len=FALSE)[[1]]
      }
    }
    plotSelect(aselects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of age selectivity was done.  \n")
  }


  #-------- PLOT likelihodd if multiplot=T ------------
  if(multiplot==TRUE){
#!!!!!!    plotLL.ss(repfile,repfile.legend=repfile.legend)  # temporary change here !!!
    plotLL.ss(repfile,repfile.legend=repfile.legend,multilambda=multilambda)
    cat("Plot of likelihood was done.  \n")    
  }

  #-------- Length diagnostics -------------------------
  if(length.on==TRUE){
    tb.tmp[[1]] <- tb0
    cl.tmp[[1]] <- cl0
    comps <- list()
    for(i in 1:length(repfile)){
      tmp <- readcompfile(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]],
                          compfile=compfile[i])
      comps[[i]] <- tmp[[1]]
    }

    plot.effN(comps)
    
    kind.var <- unique(comps[[1]]$kind)
    kind.var <- kind.var[kind.var!=""]
    for(kk in 1:length(kind.var)){
      plotlength.fit(repfile,comps=comps,compfile=compfile,tb=tb.tmp,cl=cl.tmp,
                     len.residual.plot=len.residual.plot,nma=nma,kind=kind.var[kk])
    }
    sum.length <- NA
  }

  if(names(dev.cur())!="null device") dev.off()
  if(dev.type!="x11"){
    cat("All plots were finished.  Open the file named ",paste(outfile,".",dev.type,sep=""),".  \n")
  }
  else{
    cat("All plots were finished.")
  }

#  par(org.par)
  invisible(list(biom=biom,SPR=SPR,naa=naa,faa=faa,cpue=cpue,
                 nma=nma,alk=alk,repfile=repfile,caa=caa,BRP=BRPs,
                 sum.length=ifelse(length.on==T,sum.length,NA)))
}
