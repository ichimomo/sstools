\name{branov.eq.pre2}
\alias{branov.eq.pre2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
branov.eq.pre2(x, n.age, naat.pred.vec, select.para, waa, nmaa)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{n.age}{
%%     ~~Describe \code{n.age} here~~
}
  \item{naat.pred.vec}{
%%     ~~Describe \code{naat.pred.vec} here~~
}
  \item{select.para}{
%%     ~~Describe \code{select.para} here~~
}
  \item{waa}{
%%     ~~Describe \code{waa} here~~
}
  \item{nmaa}{
%%     ~~Describe \code{nmaa} here~~
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
function(x,n.age,naat.pred.vec,select.para,waa,nmaa){
#        n.age <- 1:n.age
        tc <- (naat.pred.vec*waa*select.para*x)/
			(select.para*x+nmaa)*(1-exp(-select.para*x-nmaa))
		
        tc/1000
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
