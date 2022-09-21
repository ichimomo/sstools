\name{calc.total.catch}
\alias{calc.total.catch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.total.catch(Fvec, Mvec, Nvec, Wvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Fvec}{
%%     ~~Describe \code{Fvec} here~~
}
  \item{Mvec}{
%%     ~~Describe \code{Mvec} here~~
}
  \item{Nvec}{
%%     ~~Describe \code{Nvec} here~~
}
  \item{Wvec}{
%%     ~~Describe \code{Wvec} here~~
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
function(Fvec,Mvec,Nvec,Wvec){
  n <- length(Mvec)
  tc <- 0
  for(i in c(1:n)){
    tc <- tc + (Fvec[i]/(Fvec[i]+Mvec[i])*(1-exp(-Fvec[i]-Mvec[i])))*Nvec[i]*Wvec[i]/1000
  }
  tc
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
