\name{overplot.graph}
\alias{overplot.graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
overplot.graph(mat, col.var = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(mat,col.var=NULL,...){
  y <- apply(mat,1,sum)
  x <- as.numeric(names(y))

  if(length(x)==0) x <- 1:length(y)
  
  plot(x,y,type="n",ylim=c(0,max(y,na.rm=T)),...)

  if(is.null(col.var)) col.var <- gray(seq(from=0.1,to=0.9,length=ncol(mat)))

  mat <- cbind(0,mat)
  s <- rep(0,nrow(mat))
  
  for(i in 2:ncol(mat)){
    polygon(c(x,rev(x)),c(rev(s),s <- s+rev(mat[,i])),col=col.var[i])
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
