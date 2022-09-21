\name{solve.baranov}
\alias{solve.baranov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solve.baranov(nage, naat.pre, select.para, waa, nmaa, CHS)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nage}{
%%     ~~Describe \code{nage} here~~
}
  \item{naat.pre}{
%%     ~~Describe \code{naat.pre} here~~
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
  \item{CHS}{
%%     ~~Describe \code{CHS} here~~
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
function(nage,naat.pre,select.para,waa,nmaa,CHS){
.C("slvbaranov",
arg1=as.integer(nage),
arg2=as.double(naat.pre),
arg3=as.double(select.para),
arg4=as.double(waa),
arg5=as.double(nmaa),
arg6=as.double(CHS),
arg7=double(1))$arg7
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
