\name{count.next.line2}
\alias{count.next.line2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
count.next.line2(current.line, skip.line, table.property)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{current.line}{
%%     ~~Describe \code{current.line} here~~
}
  \item{skip.line}{
%%     ~~Describe \code{skip.line} here~~
}
  \item{table.property}{
%%     ~~Describe \code{table.property} here~~
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
function(current.line,skip.line,table.property){
  k <- k0 <- current.line+skip.line
  k <- k+1
  while(table.property[k]==table.property[k0] & k<=length(table.property)){
    k <- k+1
  }
  k-1
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
