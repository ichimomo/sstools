\name{getAgecomp.ss2}
\alias{getAgecomp.ss2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAgecomp.ss2(repfile = "ss2.rep", tb = NULL, cl = NULL, len = T, compfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{tb}{
%%     ~~Describe \code{tb} here~~
}
  \item{cl}{
%%     ~~Describe \code{cl} here~~
}
  \item{len}{
%%     ~~Describe \code{len} here~~
}
  \item{compfile}{
%%     ~~Describe \code{compfile} here~~
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
function(repfile="ss2.rep",tb=NULL,
                                            cl=NULL,len=T,compfile=NULL){#target.line=NULL,
#  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,target.line=target.line[1],compfile=compfile)  
#  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,target.line=target.line[2],len=len)

  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,compfile=compfile)  
  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,len=len)
  list(composition.database[[1]],fit.len.comps[[1]],
       c(composition.database[[2]],fit.len.comps[[2]]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
