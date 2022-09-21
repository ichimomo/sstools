\name{good.colors}
\alias{good.colors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
good.colors()
}
%- maybe also 'usage' for other objects documented here.
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
function(){
  tmp1 <- apply(col2rgb(colors()),2,max)<230
#  tmp1 <- TRUE
  tmp2 <- apply(col2rgb(colors()),2,sd)/apply(col2rgb(colors()),2,mean)>0.5
  col.tmp <- colors()[tmp1&tmp2&!is.na(tmp1)&!is.na(tmp2)]
  a <- col2rgb(col.tmp)
  col.tmp <- col.tmp[order(a[1,])]
  col.tmp <- matrix(col.tmp,3,floor(length(col.tmp)/2),byrow=T)
  dim(col.tmp) <- NULL
  col.tmp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
