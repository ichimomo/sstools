\name{getAgecomp.ss.1}
\alias{getAgecomp.ss.1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAgecomp.ss.1(repfile = "ss2.rep", cl = NULL, tb = NULL, len = TRUE)
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
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                #target.line=NULL,
                                                len=TRUE){
  desc <- ifelse(len==TRUE,"FIT_LEN_COMPS","FIT_AGE_COMPS")
#  if(is.null(target.line)){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  res <- find.and.read.table2(desc,skipline=1,gyou=NULL,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
#}
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)
#    res <- find.and.read.table(desc,skipline=1,startpoint=target.line-50,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
#  }
  if(len==TRUE){
    colnames(res[[1]]) <- c("Index","Year","Seas","Gender","Mkt","Nsamp","effN","Like")
  }
  else{
    colnames(res[[1]]) <- 
      c("Index","Year","Seas","Gender","Mkt","Ageerr","Lbin_lo","Lbin_hi","Nsamp","effN","Like")
  }
  res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
