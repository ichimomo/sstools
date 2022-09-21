\name{to.yrmonday}
\alias{to.yrmonday}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
to.yrmonday(xyz, split = "/")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyz}{
%%     ~~Describe \code{xyz} here~~
}
  \item{split}{
%%     ~~Describe \code{split} here~~
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
function(xyz,split="/"){
  a <- strsplit(as.character(xyz),split=split)
  tmpfunc <- function(vec,n){vec[n]}
  x <- lapply(a,tmpfunc,1)
  y <- lapply(a,tmpfunc,2)
  z <- lapply(a,tmpfunc,3)

  data.frame(year=unfactor(x),month=unfactor(y),day=unfactor(z))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
