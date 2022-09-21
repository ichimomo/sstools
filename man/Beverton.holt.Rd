\name{Beverton.holt}
\alias{Beverton.holt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Beverton.holt(SSB, coef, st = TRUE, B0 = 60000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SSB}{
%%     ~~Describe \code{SSB} here~~
}
  \item{coef}{
%%     ~~Describe \code{coef} here~~
}
  \item{st}{
%%     ~~Describe \code{st} here~~
}
  \item{B0}{
%%     ~~Describe \code{B0} here~~
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
function(SSB,coef,st=TRUE,B0=60000){
  recruit <- 0
  if(st==FALSE){
    recruit <- coef[1]*SSB/(coef[2]+SSB)
  }
  else{
    coef.temp <- Beverton.holt.revised2(coef,B0=B0)
    #cat(coef.temp,"\n",coef,"\n")
    recruit <- coef.temp[1]*SSB/(coef.temp[2]+SSB)
  }
  recruit
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
