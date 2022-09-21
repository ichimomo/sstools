\name{plotTotcatch}
\alias{plotTotcatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotTotcatch(biom, biom.list = NULL, col.var = rep(1, 1 + length(biom.list)), lty.var = rep(1, 1 + length(biom.list)), lwd.var = rep(1, 1 + length(biom.list)), ptype = "b", nline = -1, byyear = TRUE, titlename = "Total catch", Total.plot = FALSE, ylab.tmp = "Total catch", findseq = "ret_catch", FUN = sum, plot.obscatch = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{biom}{
%%     ~~Describe \code{biom} here~~
}
  \item{biom.list}{
%%     ~~Describe \code{biom.list} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{lty.var}{
%%     ~~Describe \code{lty.var} here~~
}
  \item{lwd.var}{
%%     ~~Describe \code{lwd.var} here~~
}
  \item{ptype}{
%%     ~~Describe \code{ptype} here~~
}
  \item{nline}{
%%     ~~Describe \code{nline} here~~
}
  \item{byyear}{
%%     ~~Describe \code{byyear} here~~
}
  \item{titlename}{
%%     ~~Describe \code{titlename} here~~
}
  \item{Total.plot}{
%%     ~~Describe \code{Total.plot} here~~
}
  \item{ylab.tmp}{
%%     ~~Describe \code{ylab.tmp} here~~
}
  \item{findseq}{
%%     ~~Describe \code{findseq} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{plot.obscatch}{
%%     ~~Describe \code{plot.obscatch} here~~
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
function(biom,biom.list=NULL,
                         col.var=rep(1,1+length(biom.list)),lty.var=rep(1,1+length(biom.list)),
                         lwd.var=rep(1,1+length(biom.list)),ptype="b",nline=-1,
                         byyear=TRUE,titlename="Total catch",Total.plot=FALSE,
                         ylab.tmp="Total catch",findseq="ret_catch",FUN=sum,plot.obscatch=FALSE){
  nfile <- 1
  islist <- is.list(biom.list)
  if(islist) ptype <- "l"

  totcatch <- 0

  target.cols <- as.list(numeric())
  target.cols[[1]] <- which(substr(names(biom),1,nchar(findseq[1]))==findseq[1])

  if(length(target.cols[[1]])==0 && length(findseq)>1){
    s <- 2
    while(length(target.cols[[1]])==0 && s<=length(findseq)){
      target.cols[[1]] <- which(substr(names(biom),1,nchar(findseq[s]))==findseq[s])
      s <- s+1
    }
  }

  res.summary <- list(numeric())
     
  if(islist){
#    for(i in 1:(length(findseq)-1)){
    for(i in 1:(length(biom.list))){    
#      target.cols[[i+1]] <- which(substr(names(biom.list[[i]]),1,nchar(findseq[i+1]))==findseq[i+1])
      target.cols[[i+1]] <- which(substr(names(biom.list[[i]]),1,nchar(findseq[1]))==findseq[1])      
      if(length(target.cols[[i+1]])==0 && length(findseq)>1){
        s <- 2
        while(length(target.cols[[i+1]])==0 && s<=length(findseq)){
          target.cols[[i+1]] <- which(substr(names(biom),1,nchar(findseq[s]))==findseq[s])
          s <- s+1
        }
      }
    }}
  
  ncatch <- length(target.cols[[1]])
  setncol(ncatch)
  s <- 1
  for(i in 1:ncatch){
#    if(vnumber>=2){ 
      tmp2.1 <- biom$period=="TIME"  & biom[,target.cols[[1]][i]]!="--"
      tmp2.2 <- biom$period=="FORE"  & biom[,target.cols[[1]][i]]!="--"
      tmp2 <- tmp2.1|tmp2.2          
#    }
#    else{
      if(sum(tmp2)==0){  # for the version older than 2.00
        tmp2 <- biom[,i]!="--" & biom$season!="E"
      }
    if(byyear==TRUE){
      y <- tapply(as.numeric(as.vector(biom[tmp2,target.cols[[1]][i]])),biom$year[tmp2],FUN)
      totcatch <- y+totcatch
      x.year <- unique(biom$year[tmp2])
      plot(x.year,y,
           type=ptype[1],lwd=lwd.var[1],ylab=ylab.tmp,xlab="Year")

      # record plotted values
#      browser()
      if(i==1){
        res.summary[[1]] <- array(0,dim=c(length(x.year),ncatch,ifelse(plot.obscatch==TRUE,2,1)))
        dimnames(res.summary[[1]]) <- list(x.year,1:ncatch,NULL)
      }
      res.summary[[1]][,i,1] <- y
      
      if(plot.obscatch){  #
        plus.factor <- ifelse(substr(findseq[1],1,9)=="ret_catch",
                              1,ifelse(findseq[1]=="retain(N)",1,4))
        y <- tapply(as.numeric(as.vector(biom[tmp2,target.cols[[1]][i]+plus.factor])),
                                              biom$year[tmp2],sum)
        points(x.year,y,type="l",lwd=2,col="gray")
        res.summary[[1]][,i,2] <- y
        if(sum(tmp2.2)!=0){
          points(unique(biom$year[tmp2.2]),tapply(as.numeric(as.vector(biom[tmp2.2,target.cols[[1]][i]+1])),biom$year[tmp2.2],sum),
                 type="b",lwd=lwd.var[1],pch=3)        
        }
      }      
      title(names(biom)[target.cols[[1]][i]],line=nline)
    }
    else{
      y <- biom[tmp2,target.cols[[1]][i]]
      totcatch <- y+totcatch
      x.year <- biom$year[tmp2]
      plot(x.year,y,
           ylab=ylab.tmp,xlab="Year",type=ptype[1],lwd=lwd.var[1])
      title(names(biom)[target.cols[[1]][i]],line=nline)

      # record plotted values
      if(i==1){
        res.summary[[1]] <- array(0,dim=c(length(x.year),ncatch,ifelse(plot.obscatch==TRUE,2,1)))
        dimnames(res.summary[[1]]) <- list(x.year,1:ncatch,NULL)
#        dimnames(res.summary[[1]])[[3]] <- 
      }
      res.summary[[1]][,i,1] <- y
      # done
      
      if(plot.obscatch){ # only for >2.00, plot observed catch
        y <- as.numeric(biom[tmp2,target.cols[[1]][i]+plus.factor])
        points(x.year,y,type="l",lwd=2,col="gray")
        res.summary[[1]][,i,2] <- y        
        if(sum(tmp2.2)!=0){
          points(biom$year[tmp2.2],as.numeric(biom[tmp2.2,target.cols[[1]][i]+1]),
                 type="b",lwd=lwd.var[1],pch=3)        
        }        
      }      
    }

      ## For overlapped plots
    if(islist){
      len.list <- length(biom.list)      
      for(j in 1:len.list){
          tmp2.1 <- biom.list[[j]]$period=="TIME"  & biom.list[[j]][,target.cols[[j+1]][i]]!="--"
          tmp2.2 <- biom.list[[j]]$period=="FORE"  & biom.list[[j]][,target.cols[[j+1]][i]]!="--"
          tmp2 <- tmp2.1|tmp2.2          
          if(sum(tmp2)==0){ # for the version older than 2.00
            tmp2 <- biom.list[[j]][,i]!="--" & biom.list[[j]]$season!="E"
          }
        if(byyear==TRUE){
          x.year <- unique(biom.list[[j]]$year[tmp2])
          y <- tapply(as.numeric(as.vector(biom.list[[j]][tmp2,target.cols[[j+1]][i]])),
                        biom.list[[j]]$year[tmp2],sum)
          points(x.year,y,type="l",
                 col=col.var[j+1],lty=lty.var[j+1],lwd=lwd.var[j+1])
        }
        else{
          points(x.year <- biom.list[[j]]$year[tmp2],
                 y <- as.numeric(biom.list[[j]][tmp2,target.cols[[j+1]][i]]),
                 type="l",col=col.var[j+1],lty=lty.var[j+1],lwd=lwd.var[j+1])          
        }
          
        # record plotted values
        if(i==1){
          res.summary[[j+1]] <- array(0,dim=c(length(x.year),ncatch,1))
          dimnames(res.summary[[j+1]]) <- list(x.year,1:ncatch,"exp")
        }
        res.summary[[j+1]][,i,1] <- y
          # done
        }}
      
      ## For title 
      if(ncatch>=10 && s\%\%10==0){
        mtext(titlename,side=3,line=0.5,adj=0.3,outer=T)
        #      nfile <- nfile+1
        #      makedevice(filename=titlename,dev.type=names(dev.cur()),filenum=nfile,
        #                 htmlfile=htmlfile,new=T,append=T)
        #      setncol(ncatch)
      }
      s <- s+1
    }
  invisible(list(totcatch=totcatch,res.summary=res.summary))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
