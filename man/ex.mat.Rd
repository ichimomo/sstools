\name{ex.mat}
\alias{ex.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ex.mat(mattemp, rm.zero = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mattemp}{
%%     ~~Describe \code{mattemp} here~~
}
  \item{rm.zero}{
%%     ~~Describe \code{rm.zero} here~~
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
function(mattemp,rm.zero=FALSE){
  a1 <- rep(rownames(mattemp),ncol(mattemp))
  a2 <- rep(0,n <- ncol(mattemp)*nrow(mattemp))
  for(i in c(1:n)) a2[i] <- colnames(mattemp)[ceiling(i/nrow(mattemp))]
  a3 <- c(mattemp)

  res <- rbind(as.numeric(a1),as.numeric(a2),as.numeric(a3))
  
  if(rm.zero==TRUE) res <- res[,res[3,]!=0]
  res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
