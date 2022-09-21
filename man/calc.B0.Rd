\name{calc.B0}
\alias{calc.B0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.B0(waa, nmaa, R.num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{waa}{
%%     ~~Describe \code{waa} here~~
}
  \item{nmaa}{
%%     ~~Describe \code{nmaa} here~~
}
  \item{R.num}{
%%     ~~Describe \code{R.num} here~~
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
function(waa,nmaa,R.num) {
  sum(NperR(nmaa,rep(0,length(nmaa)),0)*waa)/1000 * R.num
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
