\name{update.number}
\alias{update.number}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.number(n1, n0, a, m, f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n1}{
%%     ~~Describe \code{n1} here~~
}
  \item{n0}{
%%     ~~Describe \code{n0} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
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
function(n1,n0,a,m,f){
  for(i in 2:(a-1)){
    n1[i] <- n0[i-1]*exp(-f[i-1]-m[i-1])
  }
  n1[a] <- n0[a-1]*exp(-f[a-1]-m[a-1]) + n0[a]*exp(-f[a]-m[a])
  n1
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
