\name{getSRpara.ss2}
\alias{getSRpara.ss2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSRpara.ss2(repfile, cl = NULL, tb = NULL, target.line = NULL, qt = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{cl}{
%%     ~~Describe \code{cl} here~~
}
  \item{tb}{
%%     ~~Describe \code{tb} here~~
}
  \item{target.line}{
%%     ~~Describe \code{target.line} here~~
}
  \item{qt}{
%%     ~~Describe \code{qt} here~~
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
    SRpara[[1]] <- list(logR0=tmp[tmp[,2]=="SR_R0",3],steepness=tmp[tmp[,2]=="SR_steep",3],
                        sigmaR=tmp[tmp[,2]=="SR_sigmaR",3],
                        envlink=tmp[tmp[,2]=="SR_envlink",3],
                        logR1=tmp[tmp[,2]=="SR_R1_offset",3],
                        Future=tmp[tmp[,2]=="SR_autocorr",3],  #???
                        SSB0=biom[[1]]$SpawnBio[!is.na(biom[[1]]$SpawnBio)&biom[[1]]$period=="VIRG"])
  }
  else{  # for version older than 3.00
    SRpara[[1]] <- list(logR0=tmp[1,2],steepness=tmp[2,2],sigmaR=tmp[3,2],
                        envlink=tmp[4,2],logR1=tmp[5,2],Future=tmp[6,2],
                        SSB0=biom[[1]]$SpawnBio[1])    
  }
  SRpara[[3]] <- tmp
  SRpara
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
