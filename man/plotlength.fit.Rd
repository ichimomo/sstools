\name{plotlength.fit}
\alias{plotlength.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotlength.fit(repfile, lty.var = rep(1, length(repfile)), col.var = 1:length(repfile), lwd.var = rep(1, length(repfile)), compfile = NULL, tb = NULL, cl = NULL, len = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{lty.var}{
%%     ~~Describe \code{lty.var} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{lwd.var}{
%%     ~~Describe \code{lwd.var} here~~
}
  \item{compfile}{
%%     ~~Describe \code{compfile} here~~
}
  \item{tb}{
%%     ~~Describe \code{tb} here~~
}
  \item{cl}{
%%     ~~Describe \code{cl} here~~
}
  \item{len}{
%%     ~~Describe \code{len} here~~
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
function(repfile,lty.var=rep(1,length(repfile)),col.var=1:length(repfile),
                           lwd.var=rep(1,length(repfile)),compfile=NULL,tb=NULL,cl=NULL,
                           len=FALSE){
  set.mypar()
  multiplot <- length(repfile)>1
#  comps.target <- c(100,100)
  comps <- as.list(1:length(repfile))  
  for(i in 1:length(repfile)){
  #!!!!!!!!-------- belows are just only for SS3
    tmp <- getAgecomp.ss2(repfile[i],cl=cl[[i]],tb=tb[[i]],len=len,compfile=compfile[i])
#                          target.line=c(comps.target[1]-10,comps.target[1]-10))
    comps[[i]] <- list(tmp[[1]],tmp[[2]])
#    if(i>1) comps.target <- c(comps.target[1]-nrow(comps[[i]][[1]])-100,
#                            comps.target[2]-nrow(comps[[i]][[2]])-100)
  }

  len.data <- comps[[1]][[1]][comps[[1]][[1]]$kind==ifelse(len==TRUE,"LEN","AGE"),]
  fleet.row <- sort(unique(comps[[1]][[1]]$fleet))
  #  setncol(nfleet)
  fleet.name <- 1:length(fleet.row)
#  setncol(length(fleet.row))
 
  # Pearson residual of length data:
  # !!!!!!!! Caution: when size data is too huge, this plot make the result file too heavy!!!!!!!!
  if(0){
  if(len.residual.plot==T){
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
#            points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
            points(tmp$bin,y <- tmp$Pearson)
          }
        }
      }
    }}

    ###!!!!!!!!!!! NOT FOR USE currently !!!!!!!!!!!!!!!11
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
  length.bin <- sort(unique(len.data$bin))
  sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
  dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))
  if(multiplot){
    for(kk in 2:length(repfile)){
      sum.length[[kk]] <- sum.length[[1]]
    }
  }
    
  s <- 1
  row.name <- paste("F:",len.data$fleet,"-Y:",floor(as.numeric(len.data$year)),"-S:",len.data$season,sep="")
  tmp2 <- unique(row.name)
  tmp.ref <- paste("F:",comps[[1]][[2]]$Index,
                   "-Y:",floor(as.numeric(comps[[1]][[2]]$Year)),
                   "-S:",comps[[1]][[2]]$Seas,sep="")
  par(mar=c(1,1,1,1),mfcol=c(8,4))  
  for(j in 1:length(fleet.row)){
    for(i in 1:length(tmp2)){
      row.tmp <- row.name==tmp2[i]
      if(sum(len.data$fleet[row.tmp]==fleet.row[j])){
        matplot(x <- len.data$bin[row.tmp],
                y <- cbind(len.data$obs[row.tmp],
                           len.data$exp[row.tmp])*comps[[1]][[2]]$Nsamp[tmp.ref==tmp2[i]],
                col=c("royalblue",1),type=c("b","l"),                
                pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")
        ## Sum up size data by fisheries
        y <- y[!is.na(y[,1]),]
        x <- x[!is.na(x)]
        sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
        ##
        title(main=paste(tmp2[i]),line=-0.7,adj=0.1,cex.main=0.7)
      
        ## For multiple plots ##
        if(multiplot){
            for(k in 2:length(repfile)){
#              browser()
#              b.tmp <- comps[[k]]
              b.tmp <- comps[[k]]#[comps[[k]][[1]]$kind==ifelse(len==TRUE,"LEN","AGE"),]
              b.tmp[[1]] <- b.tmp[[1]][b.tmp[[1]]$kind==ifelse(len==TRUE,"LEN","AGE"),]
              row.name2 <- paste("F:",b.tmp[[1]]$fleet,
                                "-Y:",floor(as.numeric(b.tmp[[1]]$year)),
                                "-S:",len.data$season,sep="")
              tmp2 <- unique(row.name)
              tmp.ref2 <- paste("F:",b.tmp[[2]]$Index,
                               "-Y:",floor(as.numeric(b.tmp[[2]]$Year)),
                               "-S:",b.tmp[[2]]$Seas,sep="")              
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
        if(s\%\%32==0){
#          browser()
          mtext(side=3,line=0.5,adj=0.3,
                "Length fit (by each sampling, line: expected, line+circle: observed)",outer=T)
#            nfile <- nfile+1
#            makedevice(filename="length_fit",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                       new=T,append=T,width=700)
#            par(mfrow=c(5,5),mar=c(1.7,1.7,1.4,0.5),oma=c(0,0,3,3),ps=14)#mgp=c(2,1,0),ps=14)   
        }
        s <- s+1                        
        
      }
        ##

    }}
  cat("Plot of expected and observed length frequency by each observation was done.  \n")
    
#  par(mfrow=c(length(fleet.row),1))
    nfile <- 1
#    makedevice(filename="length_fit_all",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#               new=T,append=T)    
    n <- dim(sum.length[[1]])[[2]]
    setncol(n)  
    for(i in 1:n){
      matplot(x <- length.bin,y <- sum.length[[1]][,i,],type=c("b","l"),lty=1,
#              pch=1,ylab="nsmple",col=c("black","royalblue"),lwd=lwd.var[1],xlab="Length (cm)")
              pch=1,ylab="nsmple",col=c("black","royalblue"),xlab="Length (cm)")              
#      title(paste("Fleet",fleet.row[i],":",fleet.name[fleet.row[i]]),line=nline)
      title(paste("Fleet",fleet.row[i]))
      
      if(multiplot){
          for(k in 2:length(repfile)){
            points(length.bin,sum.length[[k]][,i,2],
                   type="l",col=col.var[k],lty=lty.var[k],lwd=lwd.var[k])
          }}
     
      
      if(n>10 && i\%\%32==0){
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
    col.tmp2 <- c(NA,"darkblue")    
    for(j in 1:length(fleet.row)){
      xxx <- len.data$fleet==j
      if(sum(xxx)>0){
        with(len.data[xxx,],symbols(as.numeric(year),bin,circles=sqrt(abs(x <- Pearson))/8,
                                    fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,
                                    inches=FALSE,ylim=yrange,xlim=as.numeric(xrange)))
#                          inches=max.res.fleet[names(max.res.fleet)==fleet.row[j]]/100))
#      title(paste("Fleet",j,":",fleet.name[j]),line=nline)
        title(paste("Fleet",j))
      }
      if(j\%\%3==0) mtext("Bubble plot of Pearson residuals, darkblue (obs<exp), black (obs>exp)",outer=T)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
