\name{moving.mean}
\alias{moving.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moving.mean(y, y.name = NULL, kukan = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{y.name}{
%%     ~~Describe \code{y.name} here~~
}
  \item{kukan}{
%%     ~~Describe \code{kukan} here~~
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
function(y,y.name=NULL,kukan=3){
  m <- (kukan-1)/2
  res <- res.name <- rep(0,length(y)-kukan+1)
  s <- 1
  for(i in (m+1):(length(y)-m)){
    res[s] <- mean(y[(i-m):(i+m)])
    if(!is.null(y.name)){
      res.name[s] <- y.name[i]
    }
    s <- s+1    
  }
  if(is.null(y.name)) res.name <- 1:length(res)
  list(x=as.numeric(res.name),y=res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
