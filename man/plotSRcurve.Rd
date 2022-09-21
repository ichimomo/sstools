\name{plotSRcurve}
\alias{plotSRcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotSRcurve(repfile = NULL, SRfunc = NULL, parameter = NULL, add = FALSE, col.var = 1, plotVB = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{SRfunc}{
%%     ~~Describe \code{SRfunc} here~~
}
  \item{parameter}{
%%     ~~Describe \code{parameter} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{plotVB}{
%%     ~~Describe \code{plotVB} here~~
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
function(repfile=NULL,SRfunc=NULL,parameter=NULL,add=FALSE,col.var=1,plotVB=FALSE){
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
         xlab="Spawning biomass",ylab="Recruitment",
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
  if(sum(colnames(SRfunc)=="era")>0){
    legend("topright",legend=unique(SRfunc$era[x.tmp]),pch=as.numeric(unique(SRfunc$era[x.tmp])))
  }  
  invisible(list(SRfunc=SRfunc,parameter=parameter))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
