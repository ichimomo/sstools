\name{plot.snail}
\alias{plot.snail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.snail(repfile = "ss2.rep", qt = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
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
function(repfile="ss2.rep",qt=4){
  faa <- calFAA.ss2(repfile,qt=qt,is.plot=F)
  tmp <- getSPR.ss2(repfile)[[1]]
#  ssb <- tmp$"Bio_Smry"
  ssb <- tmp$"SPB"  
  names(ssb) <- tmp$Year
  res <- list(faa=apply(faa$faa,1,mean),ssb=ssb)
  plot(res$faa,res$ssb,type="b",xlab="Average F",ylab="SSB")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
