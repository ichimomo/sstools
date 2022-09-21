\name{calc.moon.age}
\alias{calc.moon.age}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.moon.age(yr, mon, day)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{yr}{
%%     ~~Describe \code{yr} here~~
}
  \item{mon}{
%%     ~~Describe \code{mon} here~~
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
function(yr,mon,day){
  library(date)
  time <- (as.numeric(mdy.date(mon,day,yr))-as.numeric(mdy.date(1,1,1970))) # dates from 1970/1/1
  cyc <- 29.530589
  jul <- 2440587.5 + time
  k <- floor((jul-2451550.09765) / cyc) # floor
  t <- k / 1236.85
  sg <- 2451550.09765 + cyc * k + 0.0001337 * t * t - 0.40720 * sin((201.5643 + 385.8169 * k)* pi/180) + 0.17241 * sin(( 2.5534 + 29.1054 * k)* pi/180)
  moon <- jul - sg
  moonage <- floor(moon*100)/100
  moonage
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
