\name{y.per.recruits}
\alias{y.per.recruits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
y.per.recruits(maa, waa, saa, fs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maa}{
%%     ~~Describe \code{maa} here~~
}
  \item{waa}{
%%     ~~Describe \code{waa} here~~
}
  \item{saa}{
%%     ~~Describe \code{saa} here~~
}
  \item{fs}{
%%     ~~Describe \code{fs} here~~
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
function(maa,waa,saa,fs){
  wcs <- rep(0,length(fs))
  for(j in c(1:length(fs))) wcs[j] <- y.per.recruit(maa,waa,saa,fs[j])

  wcs
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
