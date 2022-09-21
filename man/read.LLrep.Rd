\name{read.LLrep}
\alias{read.LLrep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.LLrep(repfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
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
function(repfile){
  cl <- count.fields(repfile,blank.lines.skip=FALSE)
  TLL <- find.and.read.table("LIKELIHOOD",skipline=0,startpoint=0,
                             table.property=cl,comment.char="",fill=T,gyou=NULL,
                             outfile=repfile,h=FALSE,is.ss2=TRUE)

  if(!is.ss3(repfile)){
    LLs <- find.and.read.table("Forecast_Recruitment",skipline=2,startpoint=0,
                               table.property=cl,comment.char="",fill=T,gyou=NULL,
                               outfile=repfile,h=TRUE,is.ss2=TRUE)
  }
  else{
    LLs <- find.and.read.table("Fleet:",skipline=0,startpoint=0,
                               table.property=cl,comment.char="",fill=T,gyou=NULL,
                               outfile=repfile,h=TRUE,is.ss2=TRUE)    
  }

  list(TLL=TLL[[1]],LLs=LLs[[1]])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
