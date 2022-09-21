\name{make.pointmap}
\alias{make.pointmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.pointmap(x2, cex = 1.5, limit = c(0, 0.2, 0.5, 1, 5, 10))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x2}{
%%     ~~Describe \code{x2} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
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
function(x2,cex=1.5,limit=c(0,0.2,0.5,1,5,10)){
  library(RColorBrewer)
  col.tmp <- c(2,3,4,6,8,9)
  xtmp <- as.numeric(x2[3,]>limit[1])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[2])  
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[3])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[4])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[5])
  xtmp <- xtmp + as.numeric(x2[3,]>=limit[6] )
  tmp <- x2[3,]!=0 & !is.na(x2[3,]) & x2[3,]!=Inf
  points(x2[2,tmp]+0.5,x2[1,tmp]+0.5,col=brewer.pal(9,"Reds")[col.tmp[xtmp[tmp]]],pch=15,cex=cex)
  tmp <- x2[3,]==0 & !is.na(x2[3,]) & x2[3,]!=Inf
  points(x2[2,tmp]+0.5,x2[1,tmp]+0.5,pch=4,cex=cex,col="skyblue")
#  legend(360-150,50,col=brewer.pal(9,"Reds")[col.tmp],pch=15,cex=1,legend=paste(">",limit,sep=""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
