\name{mvfile}
\alias{mvfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mvfile(filename.stored, n.ss2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename.stored}{
%%     ~~Describe \code{filename.stored} here~~
}
  \item{n.ss2}{
%%     ~~Describe \code{n.ss2} here~~
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
function(filename.stored,n.ss2){
  if(.Platform$OS.type=="unix"){          
    system(paste("cp ss2.rep ","ss2",filename.stored[2],"-",n.ss2,".rep",sep=""))
    system(paste("mv ss2",filename.stored[2],"-",n.ss2,".rep ",filename.stored[1],sep=""))
    system(paste("cp ss2.par ","ss2",filename.stored[2],"-",n.ss2,".par",sep=""))
    system(paste("mv ss2",filename.stored[2],"-",n.ss2,".par ",filename.stored[1],sep=""))    
  }
  else{
    shell(paste("copy ss2.rep ","ss2",filename.stored[2],"-",n.ss2,".rep",sep=""))
    shell(paste("move ss2",filename.stored[2],"-",n.ss2,".rep ",filename.stored[1],sep=""))
    shell(paste("copy ss2.par ","ss2",filename.stored[2],"-",n.ss2,".par",sep=""))
    shell(paste("move ss2",filename.stored[2],"-",n.ss2,".par ",filename.stored[1],sep=""))    
  }
  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
