\name{calc.pcaa.tmp}
\alias{calc.pcaa.tmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.pcaa.tmp(er1, maa1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{er1}{
%%     ~~Describe \code{er1} here~~
}
  \item{maa1}{
%%     ~~Describe \code{maa1} here~~
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
function(er1,maa1){
  tmpfunc <- function(er1,maa1,pf1){
    (er1-pf1/(pf1+maa1)*(1-exp(-pf1-maa1)))^2
  }
  optimize(tmpfunc,lower=0,upper=5,er1=er1,maa=maa1)$minimum
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
