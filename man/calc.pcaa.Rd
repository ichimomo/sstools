\name{calc.pcaa}
\alias{calc.pcaa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.pcaa(caa1, naa1, maa1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{caa1}{
%%     ~~Describe \code{caa1} here~~
}
  \item{naa1}{
%%     ~~Describe \code{naa1} here~~
}
  \item{maa1}{
%%     ~~Describe \code{maa1} here~~
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
function(caa1,naa1,maa1){

  exp.rate <- pcaa <- caa1
  for(k in 1:dim(caa1)[3]){
    for(i in 1:dim(caa1)[1])
      for(j in 1:dim(caa1)[2]){
        if(caa1[i,j,k]!=0){
          exp.rate[i,j,k] <- caa1[i,j,k]/naa1[i,j]
          pcaa[i,j,k] <- calc.pcaa.tmp(exp.rate[i,j,k],maa1[j])
        }
        else{
          pcaa[i,j,k] <- 0
        }
      }
    cat(k," ")
  }
  pcaa
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
