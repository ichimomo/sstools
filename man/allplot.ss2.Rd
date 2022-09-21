\name{allplot.ss}
\alias{allplot.ss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
allplot.ss(repfile = "Report.sso", outfile = "out", is.plot.cohortSSB = TRUE, dev.type = "pdf", len.residual.plot = F, agecomp.on = FALSE, length.on = TRUE, aselect.on = F, select.on = TRUE, col.var = NULL, lty.var = NULL, diff.target = 100, lwd.var = NULL, true.value = NULL, repfile.legend = NULL, compfile = "CompReport.sso", refpoint.year = NULL, datfile = NULL, plot.resid = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{is.plot.cohortSSB}{
%%     ~~Describe \code{is.plot.cohortSSB} here~~
}
  \item{dev.type}{
%%     ~~Describe \code{dev.type} here~~
}
  \item{len.residual.plot}{
%%     ~~Describe \code{len.residual.plot} here~~
}
  \item{agecomp.on}{
%%     ~~Describe \code{agecomp.on} here~~
}
  \item{length.on}{
%%     ~~Describe \code{length.on} here~~
}
  \item{aselect.on}{
%%     ~~Describe \code{aselect.on} here~~
}
  \item{select.on}{
%%     ~~Describe \code{select.on} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{lty.var}{
%%     ~~Describe \code{lty.var} here~~
}
  \item{diff.target}{
%%     ~~Describe \code{diff.target} here~~
}
  \item{lwd.var}{
%%     ~~Describe \code{lwd.var} here~~
}
  \item{true.value}{
%%     ~~Describe \code{true.value} here~~
}
  \item{repfile.legend}{
%%     ~~Describe \code{repfile.legend} here~~
}
  \item{compfile}{
%%     ~~Describe \code{compfile} here~~
}
  \item{refpoint.year}{
%%     ~~Describe \code{refpoint.year} here~~
}
  \item{datfile}{
%%     ~~Describe \code{datfile} here~~
}
  \item{plot.resid}{
%%     ~~Describe \code{plot.resid} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(repfile="Report.sso",outfile="out",is.plot.cohortSSB=TRUE,
                                      dev.type="pdf",len.residual.plot=F,
                                      agecomp.on=FALSE,length.on=TRUE,aselect.on=F,select.on=TRUE,
                                      col.var=NULL,lty.var=NULL,diff.target=100,
                                      lwd.var=NULL,true.value=NULL,
                                      repfile.legend=NULL,compfile="CompReport.sso",
                                      refpoint.year=NULL,  # If you want to calculate reference point, input years of reference F for selectivivty, such as 2004:2006.  This option needs optional functions distributed by Yukio Takeuchi (yukiot@fra.affrc.go.jp).  
                                      datfile=NULL,plot.resid=F){

  #!!!! Known bugs !!!!!
  # *** Not work if 1st option of F method (using Pope's approximation)
  #        * please tentatively replace 'F:' in my code with 'Hrate:' or 'Hrate:' with 'F:' in report file.  
  
  nline <- 0.5

  #-------------  error check if specified files exist ---
  tmp <- which(file.info(repfile)$size==0 | is.na(file.info(repfile)$size))
  if(length(tmp)>0){
    stop(message=paste("ERROR:: no file or no data in repfile exits in ", repfile[tmp],"\n",sep=""))
  }

  if(length.on==TRUE){
    if(is.null(compfile) && is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      stop(message="ERROR:: Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"! for plotting length or age composition data with option of length.on=T")
    }
    if(is.ss3(repfile)){
      tmp <- which(file.info(compfile)$size==0 | is.na(file.info(compfile)$size))
      if(length(tmp)>0){
        stop(message=paste("ERROR:: no file or no data in compreport exits in ", repfile[tmp],"\n",sep=""))
      }}
  }
  #------------- error check done

  if(is.list(repfile)){
    repfile <- unlist(repfile)
  }

  #------------ SET for ubnormal termination 
  tmpfunc <- function(){
    if(names(dev.cur())!="null device") dev.off()
#    par(org.par)
  }
  on.exit(tmpfunc())

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
  if(dev.type=="pdf") pdf(paste(outfile,".pdf",sep=""),height=9,width=9,paper="a4")
  if(dev.type=="x11") x11()

  #---------- SET graphical parameters
  org.par <- par()
  set.mypar()
  par(font.main=1,cex.main=1,ps=12)
  multiplot <- length(repfile)>1

  #---------- READ report files
  cl0 <- count.fields(repfile[1],blank.lines.skip=FALSE)
  tb0 <- read.table(repfile[1],fill=TRUE,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                    blank.lines.skip=FALSE,colClasses=rep("character",max(cl0)))
#                    nrows=ifelse(length.on==T,-1,length(cl0)/2))[,1:2]

  tmp <- getBabs.ss2(repfile[1],cl=cl0,tb=tb0)
  biom <- list(tmp[[1]])
#  biom.target <- tmp[[2]]
  
  tmp <- getSPR.ss2(repfile[1],cl=cl0,tb=tb0)
  SPR <- list(tmp[[1]])
#  SPR.target <- tmp[[2]]    

  tmp <- getCPUE.ss2(repfile[1],cl=cl0,tb=tb0)
  cpue <- list(tmp[[1]])
#  cpue.target <- tmp[[2]]

  tmp <- getNAA.ss2(repfile[1],cl=cl0,tb=tb0)
  naa <- list(tmp[[1]])
#  naa.target <- tmp[[2]]

  tmp <- getCAA.ss2(repfile[1],cl=cl0,tb=tb0)
  caa <- as.list(numeric())
  caa[[1]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])

  tmp <- getNMA.ss2(repfile[1],cl=cl0,tb=tb0)
  nma <- list(tmp[[1]])
#  nma.target <- tmp[[2]]  

  tmp <- getALK.ss2(repfile[1],cl=cl0,tb=tb0)
  alk <- list(tmp[[1]])
#  alk.target <- tmp[[2]]

  tmp <- getSRfunc.ss2(repfile[1],cl=cl0,tb=tb0)
  SRfunc <- list(tmp[[1]])

  tmp <- getSRpara.ss2(repfile[1],cl=cl0,tb=tb0)
  SRpara <- list(tmp[[1]])  

  len.rep <- length(repfile)
  ptype <- "l"  
  if(multiplot){
    cl.tmp <- tb.tmp <- as.list(numeric())
    for(i in 2:len.rep){
      #biom[[i]] <- getBabs.ss2(repfile[i],
      #                              target.line=biom.target-diff.target-nrow(biom[[1]]))[[1]]
      #SPR[[i]] <- getSPR.ss2(repfile[i],
      #target.line=biom.target-diff.target-nrow(SPR[[1]]))[[1]]      
      #cpue[[i]] <- getCPUE.ss2(repfile[i],
      #                              target.line=cpue.target-diff.target-nrow(cpue[[1]]))[[1]]
      #alk[[i]] <- getALK.ss2(repfile[i],target.line=alk.target-diff.target-nrow(alk[[1]]),
      #                            vnumber=vnumber)[[1]]
      #naa[[i]] <- getNAA.ss2(repfile[i],target.line=naa.target-diff.target-nrow(naa[[1]]))[[1]]
      #
      # use target.line from that from naa
      #tmp <- getCAA.ss2(repfile[i],target.line=naa.target-diff.target)
      #caa[[i]] <- list(caa=tmp[[1]],caa.array=tmp[[3]])
      #
      #nma[[i]] <- getNMA.ss2(repfile[i],target.line=nma.target-diff.target-nrow(nma[[1]]))[[1]]
      #cat("Read",repfile[i],"file\n")
      cl.tmp[[i]] <- count.fields(repfile[i],blank.lines.skip=FALSE)
      tb.tmp[[i]] <- read.table(repfile[i],fill=T,col.names=paste("V",1:max(cl.tmp[[i]]),sep=""),as.is=T,
                        blank.lines.skip=FALSE,colClasses=rep("character",max(cl.tmp[[i]])))[,1:2]
#      cat(which(tb.tmp[[i]][,1]=="TIME_SERIES"),"\n")
#      cat(max(cl.tmp[[i]]),"\n")     
#      save(tb.tmp,file="tb.tmp.R")      

      biom[[i]] <- getBabs.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      SPR[[i]] <- getSPR.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      cpue[[i]] <- getCPUE.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      alk[[i]] <- getALK.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
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
  #  makedevice(dev.type=dev.type,filenum=0,filename="Biom_SSB_REC",htmlfile=htmlfile,append=T,new=F)
  if(!multiplot){
    plotBSR(biom[[1]],NULL,
            true.value=true.value,what.plot=c(T,T,T),
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline)
  }
  else{
    plotBSR(biom[[1]],biom[2:len.rep],
            true.value=true.value,what.plot=c(T,T,T),
          col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,nline=nline)    
  }

  if(is.null(repfile.legend)){
    legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
  }
  else{
    legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)    
  }
  mtext(side=3,line=0.5,adj=0.1,"** Biomass, SSB and Recruitment **",outer=T)
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

  ## Plot estimated (or fixed) growth curve -------------------------------------
  par(mfrow=c(3,1))
  a <- as.matrix(alk[[1]])
  x <- as.numeric(colnames(a)[-1])
  tmp <- a[,1]=="mean"
  tmp2 <- a[,1]=="sdsize"  
  plot(x,as.numeric(a[tmp,-1]),type="b",xlab="Age",ylab="Length (cm)",lwd=lwd.var[1])
  points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l")
  points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l")
  abline(h=seq(from=0,to=max(as.numeric(a[tmp,-1])),by=50),col="gray")
  title("Estimated growth curve (1st season in each age)",line=nline)
  if(multiplot){
    for(i in 2:len.rep){
      a <- as.matrix(alk[[i]])
      x <- as.numeric(colnames(a)[-1])
      tmp <- a[,1]=="mean"
      tmp2 <- a[,1]=="sdsize"        
      points(x,as.numeric(a[tmp,-1]),type="b",col=col.var[i],
             lty=lty.var[i],lwd=lwd.var[i])
      points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l",
             col=col.var[i],lty=lty.var[i],lwd=lwd.var[i])
      points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l",
             col=col.var[i],lty=lty.var[i],lwd=lwd.var[i])
      }}

  ## Plot SR curve 
  #  makedevice(filename="SRcurve",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
  #             new=T,append=T)
  plotSRcurve(repfile=repfile[1],SRfunc=SRfunc[[1]],parameter=SRpara[[1]])
  if(multiplot){
    for(i in 2:len.rep){
      plotSRcurve(repfile=repfile[i],SRfunc=SRfunc[[i]],parameter=SRpara[[i]],
                  add=T,col.var=col.var[i])
    }
  }
  mtext(side=3,line=0.5,adj=0.1,"** Growth curve & Spawner-recruitment **",outer=T)
  cat("Plot of growth curve was done.\n")

  ##----------  Plot natural mortality --------------------
  x <- tapply(nma[[1]]$M,nma[[1]]$"Age_Mid",mean)
  plot(names(x),x,type="l",lwd=2,ylim=c(0,max(x)*1.1))
  if(multiplot){
    for(i in 2:len.rep){
      x <- tapply(nma[[i]]$M,nma[[i]]$"Age_Mid",mean)
      points(names(x),x,type="l",col=col.var[i],lwd=2)
    }
  }
  ##-------------------------------------------------------
  
  # legend
  if(multiplot){
#    nplot()
    if(is.null(repfile.legend)){
#      legend(1,10,legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
      legend("topright",legend=repfile,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
    }
    else{
#      legend(1,10,legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)
      legend("topright",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2)      
    }
  }
  #---------------------------------------------------------------------------------
  
  ## Plot SSB by cohorts ##
  if(is.plot.cohortSSB==TRUE){  
    for(i in 1:len.rep){
      plot.cohortSSB(repfile[i],naa=naa[[i]],nma=nma[[i]])
      par(las=1)
      mtext(side=3,line=0.5,adj=0.1,outer=T,paste("Numbers of spawning population by year-class in",repfile[i]))
    }
    cat("Plot of numbers by year class was done.\n")
  }
  
  
  ## Plot F and exploitation rates##
  #  makedevice(dev.type=dev.type,filenum=0,filename="F_and_exp",htmlfile=htmlfile,append=T,new=T)
   par(mfrow=c(3,1),mar=c(4,5,2,0),oma=c(0,0,3,3),mgp=c(2,1,0))

  if(!multiplot){
    plot.data.frame(SPR[[1]],NULL,"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"SPR",title="SPR (at R0)?",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],NULL,"Y/R",title="Y/R (at R0)?",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
  }
  else{
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Tot_Exploit",title="Total exploitation rates",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"SPR",title="SPR",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)
    plot.data.frame(SPR[[1]],SPR[2:len.rep],"Y/R",title="Y/R",nline=nline,
                    col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype)    
  }

  ## Plot F and exploitation rates (2): calculated from numbers and catch at age ##
  ## Read faa here
  ##---- !!!!!! imcomplete for SS3 !!!!!!!!!!!!!!!! --------------------  
  faa <- list(calFAA.ss2(repfile=NULL,
                         datas=list(naa=naa[[1]],caa=caa[[1]],nma=nma[[1]],biom=biom[[1]]),is.plot=T))
  mtext(side=3,line=0.5,adj=0.1,
        paste2("** Fishing impacts by age (average through quarters, in ",repfile[1],") **"),outer=T)

  if(multiplot){
    for(i in 2:len.rep){
      faa[[i]] <- calFAA.ss2(repfile=NULL,
                             datas=list(naa=naa[[i]],caa=caa[[i]],nma=nma[[i]],biom=biom[[i]]),
                             is.plot=T)
      mtext(side=3,line=0.5,adj=0.1,
            paste2("** Fishing impacts by age (average through quarters, in ",repfile[i],") **"),outer=T)
    }
  }

  # Fishing impacts by year
  nfleet <- dim(faa[[1]]$faa.array)[[3]]
  setncol(nfleet)
  plot(1:10,1:10,type="n",axes=F,xlab="",ylab="")
  #  browser()
  legend(1,10,legend=paste(1:nfleet,fleet.name,sep=":"),
         pch=1:nfleet,ncol=2,bty="n")
  years <- as.numeric(rownames(faa[[1]]$faa.array[,,1]))
  for(i in 1:dim(faa[[1]]$faa.array)[[2]]){
    x0 <- 0
    for(fleet in 1:nfleet){
      x <- tapply(faa[[1]]$faa.array[,i,fleet],floor(as.numeric(rownames(faa[[1]]$faa.array[,,fleet]))),sum)
      x0 <- max(x,x0,na.rm=T)
    }

    plot(names(x),rep(0,length(x)),ylim=c(0,x0),type="n",ylab="Continuous F",xlab="Year")
    title(paste("Age",dimnames(faa[[1]]$faa.array)[[2]][i]))
    for(fleet in 1:nfleet){
      tmp <- tapply(faa[[1]]$faa.array[,i,fleet],floor(as.numeric(rownames(faa[[1]]$faa.array[,,1]))),sum)
      if(sum(tmp,na.rm=T)>0){
        points(names(tmp),tmp,pch=fleet,type="b",cex=0.7)

        # multiplot
        if(multiplot==TRUE){
          for(k in 2:len.rep){
            tmp <- tapply(faa[[k]]$faa.array[,i,fleet],
                          floor(as.numeric(rownames(faa[[k]]$faa.array[,,1]))),sum)
            if(sum(tmp,na.rm=T)>0){
              points(names(tmp),tmp,pch=fleet,type="b",cex=0.7,col=col.var[k])
          }}
        }
        
      }
    }
    if(i\%\%10==9)   mtext(side=3,line=0.5,adj=0.1,"** F by fleet, age and year **",outer=T)
  }

  cat("Plot of estimated F and exploitation rates was done.\n")
  ##-------------------------------------------------------------------------------  

  ## Plot total catch in weight##
  char.tmp <- numeric()
  for(i in 1:len.rep){
    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"retain(B)","ret_catch")
  }
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,
                 findseq=char.tmp,titlename="Total catch in weight (gray: observed, black: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=TRUE)
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],
                 findseq=char.tmp,titlename="Total catch in weight (gray: observed, black: expected)",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=TRUE)    
  }
  cat("Plot of estimated total catches (in weight) was done.  \n")

  # total catch by number
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
               findseq=char.tmp,titlename="Total catch in number (gray: observed, black: expected)",
               col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
               plot.obscatch=TRUE)
#  }
#  else{
#    plotTotcatch(biom[[1]],biom[2:len.rep],
#                 findseq=char.tmp,titlename="Total catch in number (gray: observed, black: expected)",
#                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
#                 plot.obscatch=T)    
#  }
  cat("Plot of estimated total catches (in number) was done.  \n")  

  ## Plot Exploitation rates by fleet  (average by season)##
  #  This find seq is depending on the option of F method.  
  #  for(i in 1:len.rep){  
  #    char.tmp[i] <- ifelse(is.ss3(repfile[i]),"F:","Hrate")
  #  }
  if(!multiplot){
    plotTotcatch(biom[[1]],NULL,byyear=FALSE,
                 ylab="Exploitation rates or F",findseq=c("F:","Hrate"),FUN=mean,
                 titlename="Exploitation rates or F",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=F)
  }
  else{
    plotTotcatch(biom[[1]],biom[2:len.rep],byyear=FALSE,
                 ylab="Exploitation rates",findseq=c("F:","Hrate"),FUN=mean,
                 titlename="Exploitation rates",
                 col.var=col.var,lty.var=lty.var,lwd.var=lwd.var,ptype=ptype,nline=nline,
                 plot.obscatch=F)    
  }
#  mtext(side=3,line=0.5,adj=0.1,"Exploitation rates",outer=T)  
  cat("Plot of estimated exploitation rates was done.  \n")

  ### CPUE
  fleetn <- unique(cpue[[1]]$index)  
#  par(mfrow=c(length(fleetn),1),ps=16)

#  makedevice(filename="CPUE_fit",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#             new=T,append=T)
  nfile <- 1  
  setncol(length(fleetn))
  for(i in fleetn){
    cpue0 <- cpue[[1]]
    matplot(x <- cpue0$year[cpue0$index==i],
            y <- cbind(cpue0$obs[cpue0$index==i],cpue0$exp[cpue0$index==i]),
            pch=c(1,NA),col=c(1,1),type=c("b","l"),
            xlab="Year",ylab="CPUE",lty=c(1,1),lwd=lwd.var[1])
    title(paste("Fleet",i,":",fleet.name[i]),line=nline)    
#    legend(max(x),max(y),xjust=1,yjust=1,legend=c("Expected","Observed"),col=c(1,1),pch=c(NA,1),lty=c(1,1))

    ## For overlapped plots
    if(multiplot){
      for(j in 2:len.rep){
        cpue0 <- cpue[[j]]
        points(cpue0$year[cpue0$index==i],
               cpue0$exp[cpue0$index==i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
      }}
    ##
    
    if(length(fleetn)>10 && i\%\%10==0){
      mtext(side=3,line=nline,adj=0.3,"CPUE (line: expected, line+circle: observed)",outer=T)
      nfile <- nfile+1
#      makedevice(filename="CPUE_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                 new=T,append=T)
#      setncol(length(fleetn))
    }
  }
  
  mtext(side=3,line=2,adj=0.3,"CPUE",outer=T)
  cat("Plot of CPUE (obs vs. est) was done.  \n")  

  # Selectivity
  if(select.on==TRUE){
#    makedevice(filename="Select",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=T,append=T)
    nfile <- 1
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0)
    selects <- list(tmp[[1]])
    selects.target <- tmp[[2]]      

    if(multiplot){
      #    select.list <- as.list(rep(0,len.rep))    
      for(i in 2:len.rep){
        selects[[i]] <- getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])[[1]]
      }
    }

    plotSelect(selects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of length selectivity was done.  \n")
  }

  # Age Selectivity
  if(aselect.on==TRUE){
#    makedevice(filename="ASelect",dev.type=dev.type,filenum=0,htmlfile=htmlfile,
#               new=T,append=T)
    nfile <- 1
    
    tmp <- getSelect.ss2(repfile[1],cl=cl0,tb=tb0,len=F)
    aselects <- list(tmp[[1]])
    aselects.target <- tmp[[2]]
    
    if(multiplot){
      for(i in 2:len.rep){
        aselects[[i]] <-
          getSelect.ss2(repfile[i],cl=cl.tmp[[i]],tb=tb.tmp[[i]])
      }
    }
    
    plotSelect(aselects,multiplot=multiplot,ptype=ptype,col=col.var,lty=lty.var,lwd=lwd.var,nline=nline)
    cat("Plot of age selectivity was done.  \n")
  }


  #-------- PLOT likelihodd if multiplot=T
  if(multiplot==TRUE){
    plotLL.ss(repfile)
    cat("Plot of likelihood was done.  \n")    
  }

  #!!!!!!!-------- if SS3, use function of plotlength.fit (preliminary setting)
  ## To show observed and expected size frequency
  if(length.on==TRUE | agecomp.on==TRUE){

    #--- if SS3 -> go to modified function
    if(is.ss3(repfile[1])){
      tb.tmp[[1]] <- tb0
      cl.tmp[[1]] <- cl0      
      plotlength.fit(repfile,compfile=compfile,tb=tb.tmp,cl=cl.tmp,len=!agecomp.on)
      sum.length <- NA
    }
    else{  # reserve the program for SS2
      nfile <- 1
      tmp <- getAgecomp.ss2(repfile[1],cl=cl0,tb=tb0,len=!agecomp.on)
      comps <- list(list(tmp[[1]],tmp[[2]]))
      comps.target <- tmp[[3]]
      if(multiplot){
        #      b.list <- as.list(rep(0,len.rep))
        for(i in 2:len.rep){
          comps[[i]] <-
            getAgecomp.ss2(repfile[i],len=!agecomp.on)
        }
      }

      len.data <- comps[[1]][[1]]    
      fleet.row <- sort(unique(comps[[1]][[1]]$fleet))
      #  setncol(nfleet)
      setncol(length(fleet.row))

    # Pearson residual of length data:
    # !!!!!!!! Caution: when size data is too huge, this plot make the result file too heavy!!!!!!!!
      if(len.residual.plot==TRUE){
        #browser()
        s <- 1
        for(i in fleet.row){
          tmp <- len.data[len.data$fleet==i,]
          plot(tmp$bin,y <- tmp$Pearson,ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")
          title(main=paste("Fleet",i,":",fleet.name[i]),line=-1)
      #    sd.tmp <- (tmp$obs-tmp$exp)/sqrt(tmp$exp*(1-tmp$exp)*tmp$N)
      #    plot(tmp$bin,sd.tmp,
          if(!multiplot){
            x1 <- tapply(y,tmp$bin,median)
            x2 <- tapply(y,tmp$bin,mean)  
            abline(h=0,col="yellow")
            points(names(x1),x1,type="l",col="red")
            points(names(x2),x2,type="l",col="blue")
          }
          else{
            for(j in 2:len.rep){
              tmp <- comps[[j]][[1]][comps[[j]][[1]]$fleet==i,]
              points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])          
            }
          }
        }
      }

      ###!!!  Not for USE now 
    if(0){
      for(i in fleet.row){
        plot(x <- len.data$exp[len.data$fleet==i & !is.na(len.data$obs)],y <- len.data$obs[len.data$fleet==i & !is.na(len.data$obs)],lwd=lwd.var[1],
             ylim=c(0,max(x,y)),xlim=c(0,max(x,y)),xlab="Expectet size freq",ylab="Observed size freq")
        title(main=paste("fleet ",i),line=nline)

        if(multiplot){
          for(j in 2:len.rep){
            tmp <- comps[[j]][[1]]
            points(x <- tmp$exp[tmp$fleet==i & !is.na(tmp$obs)],
                   y <- tmp$obs[tmp$fleet==i & !is.na(tmp$obs)]
                   ,type="p",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }}
        if(length(fleet.row)>10 && s\%\%10==0){
          mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
#          nfile <- nfile+1
#          makedevice(filename="length_cor",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                     new=T,append=T)
#          setncol(length(fleet.row))
        }
        s <- s+1
      }
      mtext(side=3,line=0.5,adj=0.3,"Expected vs observed size composition",outer=T)
      cat("Scatter plot of length frequency (obs vs est) was done.  \n")
    }
    

  ## Size frequency 2
  ##!!! TOO bad algrosm, which should be shoreter in future
    length.bin <- sort(unique(len.data$bin))
    sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
    dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))
    
    if(multiplot){
#      sum.length <- as.list(rep(0,len.rep))
      for(i in 2:len.rep){
        sum.length[[i]] <- sum.length[[1]]
      }}
    
#    nfile <- 1
#    makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T,width=700)  
#    par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)
    
#   browser()
    
    s <- 1
    row.name <- paste(floor(as.numeric(len.data$year)),len.data$season,len.data$fleet,sep="-")
    tmp2 <- unique(row.name)
    tmp.ref <- paste(floor(as.numeric(comps[[1]][[2]]$Year)),comps[[1]][[2]]$Seas,comps[[1]][[2]]$Index,sep="-")
    for(j in 1:length(fleet.row)){
      for(i in 1:length(tmp2)){
        if(sum(len.data$fleet[row.name==tmp2[i]]==fleet.row[j])){
          matplot(x <- len.data$bin[row.name==tmp2[i]],
                  y <- cbind(len.data$obs[row.name==tmp2[i]],
                             len.data$exp[row.name==tmp2[i]])*comps[[1]][[2]]$Nsamp[tmp.ref==tmp2[i]],
                  lwd=lwd.var[1],col=c("royalblue",1),type=c("b","l"),
                  pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")
        ## Sum up size data by fisheries
          y <- y[!is.na(y[,1]),]
          x <- x[!is.na(x)]
          sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
          ##
          title(main=paste(tmp2[i]),line=0.5)

          ## For multiple plots ##
          if(multiplot){
            for(k in 2:len.rep){
              b.tmp <- comps[[k]]
              row.name <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
              tmp2 <- unique(row.name)
              tmp.ref <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Index,sep="-")              
#              row.name2 <- paste(floor(as.numeric(b.tmp[[1]]$year)),b.tmp[[1]]$season,b.tmp[[1]]$fleet,sep="-")
#              tmp.ref2 <- paste(floor(as.numeric(b.tmp[[2]]$Year)),b.tmp[[2]]$Seas,b.tmp[[2]]$Index,sep="-")

              x <- b.tmp[[1]]$bin[row.name2==tmp2[i]]
              y <- cbind(b.tmp[[1]]$obs[row.name2==tmp2[i]],
                         b.tmp[[1]]$exp[row.name2==tmp2[i]]) * b.tmp[[2]]$Nsamp[tmp.ref2==tmp2[i]]
              points(x,y[,2],col=col.var[k],lty=lty.var[k],lwd=lwd.var[k],type="l")
              y <- y[!is.na(y[,1]),]
              x <- x[!is.na(x)]
              sum.length[[k]][match(x,length.bin),j,] <- sum.length[[k]][match(x,length.bin),j,]+y            
            }}
        ##
          
          if(s\%\%25==0){
            mtext(side=3,line=0.5,adj=0.3,
                  "Length fit (by each sampling, line: expected, line+circle: observed)",outer=T)
#            nfile <- nfile+1
#            makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                       new=T,append=T,width=700)
#            par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)#mgp=c(2,1,0),ps=14)   
          }
          s <- s+1                
        }}}
    
  cat("Plot of expected and observed length frequency by each observation was done.  \n")
#  par(mfrow=c(length(fleet.row),1))
    nfile <- 1
#    makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T)    
      n <- dim(sum.length[[1]])[[2]]
    setncol(n)
    for(i in 1:n){
      matplot(x <- length.bin,y <- sum.length[[1]][,i,],type=c("b","l"),lty=1,
              pch=1,ylab="nsmple",col=c("black","royalblue"),lwd=lwd.var[1],xlab="Length (cm)")
      title(paste("Fleet",fleet.row[i],":",fleet.name[fleet.row[i]]),line=nline)

      if(multiplot){
        for(k in 2:len.rep){
          points(length.bin,sum.length[[k]][,i,2],
                 type="l",col=col.var[k+1],lty=lty.var[k],lwd=lwd.var[k])
          }}      
      
      if(n>10 && i\%\%10==0){
        mtext(side=3,line=0.5,adj=0.3,
              "Length fit (by fleet, line: expected, line+circle: observed)",outer=T)
#        nfile <- nfile+1
#        makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                   new=T,append=T)                
#        setncol(n)
      }}
    mtext(side=3,line=0.5,adj=0.3,"Length fit (by fleet, line: expected, line+circle: observed))",
          outer=T)
    cat("Plot of expected and observed length frequency by fleets was done.  \n")    

    # Bubble plot not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=T)    
    max.res.fleet <- tapply(abs(len.data$Pearson),len.data$fleet,max,na.rm=T)
    par(mfrow=c(3,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"black")    
    for(j in 1:length(fleet.row)){
      with(len.data[len.data$fleet==j,],symbols(year,bin,circles=sqrt(abs(x <- Pearson))/8,
                          fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                          inches=FALSE,ylim=yrange,xlim=xrange))
#                          inches=max.res.fleet[names(max.res.fleet)==fleet.row[j]]/100))
      title(paste("Fleet",j,":",fleet.name[j]),line=nline)
      if(j\%\%3==0) mtext("Bubble plot of Pearson residuals, black(obs<exp), black (obs>exp)",outer=T)
    }
    }
  }
  if(names(dev.cur())!="null device") dev.off()

  if(dev.type!="x11"){
    cat("All plots were finished.  Open the file named ",paste(outfile,".",dev.type,sep=""),".  \n")
  }
  else{
    cat("All plots were finished.")
  }

#  par(org.par)
  invisible(list(biom=biom,sum.length=ifelse(length.on==T,sum.length,NA)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
