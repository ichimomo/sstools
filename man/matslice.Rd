\name{matslice}
\alias{matslice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
matslice(mat, num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{num}{
%%     ~~Describe \code{num} here~~
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
function(mat,num){
  tmp <- rep(0,nrow(mat))
  x <- ceiling(ncol(mat)/num)
  s <- 0
  for(i in 1:x){
    if(ncol(mat)<num){
      tmp <- cbind(tmp,apply(as.matrix(mat[,1:ncol(mat)]),1,sum))
    }
    else{
      tmp <- cbind(tmp,apply(as.matrix(mat[,1:num]),1,sum))
      mat <- as.matrix(mat[,-1:-num])
      s <- s+1
#      cat(num," ",s," ")
    }

  }
  tmp <- tmp[,-1]
  rownames(tmp) <- rownames(mat)
  colnames(tmp) <- num*1:ncol(tmp)
  tmp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
