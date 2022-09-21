\name{doss3}
\alias{doss3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
doss3(how.many = 1, ss3.arg = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{how.many}{
%%     ~~Describe \code{how.many} here~~
}
  \item{ss3.arg}{
%%     ~~Describe \code{ss3.arg} here~~
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
function(how.many=1,ss3.arg=""){
  for(i in 1:how.many){
    if(.Platform$OS.type=="unix"){
      system("cp ss3.par SS3.PAR")        
      system(paste("ss3 ",ss3.arg))
    }
    else{
      shell(paste("ss3.exe ",ss3.arg))
    }
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
