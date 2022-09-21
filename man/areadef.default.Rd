\name{areadef.default}
\alias{areadef.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
areadef.default(lon, lat, lats, lons, char = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{lats}{
%%     ~~Describe \code{lats} here~~
}
  \item{lons}{
%%     ~~Describe \code{lons} here~~
}
  \item{char}{
%%     ~~Describe \code{char} here~~
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
function(lon,lat,lats,lons,char=1){
  areadef <- as.numeric(lat>=lats[1] & lat<lats[2] & lon>=lons[1] & lon<lons[2])
  areadef <- ifelse(areadef==1,char,0)
  areadef
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
