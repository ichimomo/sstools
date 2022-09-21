\name{expand.category}
\alias{expand.category}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
expand.category(num, category, label)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
%%     ~~Describe \code{num} here~~
}
  \item{category}{
%%     ~~Describe \code{category} here~~
}
  \item{label}{
%%     ~~Describe \code{label} here~~
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
function(num,category,label){
  n <- sum(num)
  res <- data.frame(label=factor(NA,level=levels(label)),category=rep(0,n))
  s <- 0
  for(i in 1:length(num)){
    range <- (s+1):(s+num[i])
    res$label[range] <- label[i]
    res$category[range] <- category[i]
    s <- s+num[i]
  }
  res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
