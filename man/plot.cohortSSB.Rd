\name{plot.cohortSSB}
\alias{plot.cohortSSB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.cohortSSB(repfile = NULL, naa = NULL, nma = NULL, age.specific.mat = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{naa}{
%%     ~~Describe \code{naa} here~~
}
  \item{nma}{
%%     ~~Describe \code{nma} here~~
}
  \item{age.specific.mat}{
%%     ~~Describe \code{age.specific.mat} here~~
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
function(repfile=NULL,naa=NULL,nma=NULL,age.specific.mat=TRUE){
  if(is.null(naa)|is.null(nma)){
    naa <- getNAA.ss2(repfile)[[1]]
    nma <- getNMA.ss2(repfile,qt=1)[[1]]
  }
  
  tmp <- nma$Seas==1
#  tmp <- nma$Season==1  
  {if(age.specific.mat==TRUE){
    maa <- nma$"Age_Mat"[tmp]  # maturity at age in using age specific maturity
  }
  else{
    maa <- nma$"Len_Mat"[tmp]  # maturity at age in using logistic
  }}

  if(!more.ss3.11(repfile)){
    ssb <- sweep(naa[tmp <- ((naa$Per=="TIME"|naa$Per=="FORE") & naa$Seas==1),which(names(naa)=="0"):ncol(naa)],2,maa,FUN="*")
  }
  else{
    ssb <- sweep(naa[tmp <- ((naa$Per=="TIME"|naa$Per=="FORE") &
         naa$Seas==1 & naa$"Beg/Mid"=="B"),which(names(naa)=="0"):ncol(naa)],2,maa,FUN="*")    
  }
  rownames(ssb) <- naa$"Year"[tmp]
  ssb <- t(as.matrix(ssb))

  col.mat <- ssb
  col.mat[] <- 0
  col.tmp <- c(terrain.colors(12)[tmp <- c(1,3,5,7,9,11,2,4,6,8,10,12)],rainbow(12)[tmp])
  #col.tmp <- c("#F7FCFD","#800026","#E5F5F9","#BD0026","#CCECE6","#E31A1C","#99D8C9","#FC4E2A","#66C2A4","#FD8D3C","#41AE76","#FEB24C","#238B45","#FED976","#006D2C","#FFEDA0","#00441B","#FFFFCC")
  for(i in -20:ncol(ssb)){
    s <- 0
    for(j in 1:nrow(ssb)){
      if(i+s<=ncol(ssb) && i+s>0){
        col.mat[j,i+s] <- col.tmp[i\%\%24+1]
      }
      s <- s+1        
    }
  }
  col.mat[col.mat==0] <- 1
#  set.mypar()

  #postscript("tmp.ps",horizontal=FALSE,height=9)
  par(mfrow=c(2,1),mar=c(3,3,1,1))
  par(las=3)
  b0 <- barplot(ssb,col="gray")
  for(i in 1:ncol(ssb)){
    ssb0  <- ssb
    ssb0[,-i]  <- NA
    barplot(ssb0,add=TRUE,col=col.mat[,i],axes=FALSE,axisname=FALSE)
    #  locator(1)
  }

  plot(1:10,type="n",axes=F,xlab="",ylab="")
  legend(1,10,fill=col.mat[1,],legend=colnames(col.mat),ncol=6)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
