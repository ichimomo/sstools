\name{month2days}
\alias{month2days}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
month2days(month, day)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{month}{
%%     ~~Describe \code{month} here~~
}
  \item{day}{
%%     ~~Describe \code{day} here~~
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
function(month,day){
  x <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  x1 <- numeric()
  if(length(month)==1) x1 <- sum(x[0:(month-1)])+day
  else{
    for(i in c(1:length(month))) x1[i] <- sum(x[0:(month[i]-1)])+day[i]
  }
  x1
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
