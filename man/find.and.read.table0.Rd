\name{find.and.read.table0}
\alias{find.and.read.table0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find.and.read.table0(findseq, skipline, startpoint, gyou, table.property, outfile, h = TRUE, is.ss2 = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{findseq}{
%%     ~~Describe \code{findseq} here~~
}
  \item{skipline}{
%%     ~~Describe \code{skipline} here~~
}
  \item{startpoint}{
%%     ~~Describe \code{startpoint} here~~
}
  \item{gyou}{
%%     ~~Describe \code{gyou} here~~
}
  \item{table.property}{
%%     ~~Describe \code{table.property} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{is.ss2}{
%%     ~~Describe \code{is.ss2} here~~
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
function(findseq,skipline,startpoint,gyou,
			table.property,outfile,h=TRUE,is.ss2=FALSE,...){

  for(i in c(startpoint:(length(table.property)-1))){
#    tmp <- read.table(outfile,skip=i-1,nrow=1)[1]
#    cat(i,":",as.character(tmp)," ")
    if(table.property[i]!=0 && read.table(outfile,skip=i-1,nrow=1)[1]==findseq){
      if(is.null(gyou)) gyou <- count.next.line(i,skipline,table.property)-i-skipline
      a <- read.table(outfile,skip=i+skipline,header=FALSE,nrow=gyou,...)
      if(h==TRUE) {
        a.name <- read.table(outfile,skip=i+skipline-1,header=FALSE,nrow=1,colClasses="character")#as.is=TRUE)
        dimnames(a) <- list(a[,1],as.character(a.name))
      }
      if(is.ss2==FALSE){
        a <- a[,-1]
      }
      startpoint <-  i + gyou
      break
    }
  }
  list(a,startpoint)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
