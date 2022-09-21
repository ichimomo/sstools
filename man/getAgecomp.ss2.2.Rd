\name{getAgecomp.ss2.2}
\alias{getAgecomp.ss2.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAgecomp.ss2.2(repfile = "ss2.rep", cl = NULL, tb = NULL, compfile = NULL)
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
  \item{compfile}{
%%     ~~Describe \code{compfile} here~~
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
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                compfile=NULL){ #target.line=NULL,
  #----- preliminary coding for adjusting 2.00o
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  if((is.ss2.2(repfile) && substr(a[1,1],16,20)>"2.00o") || (is.ss3(repfile) && vnumber.ss3(repfile)<3.03)){
    #--  for version from 2.02o to 3.02
    name.tmp <- c("year","season","fleet","rep","pick_gender",
                  "kind","mkt","ageerr","gender","Lbin_lo","Lbin_hi",
                  "bin","obs","exp","Pearson","N","effN","like","Used")
  }
  else{
    if(is.ss3(repfile) & vnumber.ss3(repfile)>=3.03){
      #--  for newer than 3.03 (read compfile) 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind","Part","ageerr","gender","Lbin_lo","Lbin_hi","bin","obs","exp","Pearson","N","effN","Like","Cum_obs","Cum_exp","Used")
      if(is.null(compfile)){
        stop(message="This version is newer than 3.30.  Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"!")
      }
      else{
        repfile <- compfile
        cl <- count.fields(repfile,blank.lines.skip=FALSE)
        tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)        
      }
    }
    else{
      #--  for version older than 2.02o 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind",
                    "mkt","ageerr","gender","Lbin_lo","Lbin_hi","bin",
                    "obs","exp","Pearson","N","effN","Used")
    }}
  type.tmp <- c("character",rep("numeric",4),"character",rep("numeric",length(name.tmp)-6))

  #----- preliminary coding for adjusting >3.00
  if(is.ss3(repfile)){
    gyou.margin <- 2
  }
  else{
    gyou.margin <- 0
  }

#  if(is.null(target.line)){
    if(is.null(cl)){
      cl <- count.fields(repfile,blank.lines.skip=FALSE)
    }
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    res <- find.and.read.table2("Composition_Database",skipline=1,
                                gyou=NULL,table.property=cl,tb=tb,gyou.margin=gyou.margin,
                                outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,
                                colClasses=type.tmp,col.names=name.tmp)
#  }
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)  
#    res <- find.and.read.table("Composition_Database",skipline=1,startpoint=target.line-50,
#                               gyou=NULL,table.property=cl,gyou.margin=gyou.margin,
#                               outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,
#                               colClasses=type.tmp,col.names=name.tmp)    
#  }
#  colnames(res[[1]]) <- name.tmp
  res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
