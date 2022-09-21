\name{bindmat}
\alias{bindmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bindmat(mat, n, row = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{row}{
%%     ~~Describe \code{row} here~~
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
function(mat,n,row=T){
  mat2 <- mat
  if(row==T){
    for(i in 1:(n-1))   mat2 <- rbind(mat2,mat)
  }
  else{
    for(i in 1:(n-1))   mat2 <- cbind(mat2,mat)
  }
  mat2
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
