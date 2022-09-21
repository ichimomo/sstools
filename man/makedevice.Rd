\name{makedevice}
\alias{makedevice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makedevice(filename = NULL, dev.type = "ps", filenum = NULL, htmlfile = NULL, new = F, append = TRUE, tmp.par = par(), width = 570, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{dev.type}{
%%     ~~Describe \code{dev.type} here~~
}
  \item{filenum}{
%%     ~~Describe \code{filenum} here~~
}
  \item{htmlfile}{
%%     ~~Describe \code{htmlfile} here~~
}
  \item{new}{
%%     ~~Describe \code{new} here~~
}
  \item{append}{
%%     ~~Describe \code{append} here~~
}
  \item{tmp.par}{
%%     ~~Describe \code{tmp.par} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(filename=NULL,dev.type="ps",filenum=NULL,htmlfile=NULL,
                       new=F,append=TRUE,tmp.par=par(),width=570,...){
  if(dev.type=="html"){
    if(new==T) dev.off()
    jpeg(file=n.tmp <- paste(filename,filenum,".jpeg",sep=""),
           quality=150,height=700,width=width,...)
#    par(tmp.par)
    cat(paste("<img src=\"./",n.tmp,"\" width=\"",width,"\" height=\"700\">\n",sep=""),file=htmlfile,append=append)
  }
  else{
 #  if(dev.type=="x11") locator(1)
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
