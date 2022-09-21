\name{find.and.read.table2}
\alias{find.and.read.table2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find.and.read.table2(findseq, skipline, gyou, comment.char = "#", table.property, tb, outfile, h = TRUE, is.ss2 = FALSE, gyou.margin = 0, target.line = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{findseq}{
%%     ~~Describe \code{findseq} here~~
}
  \item{skipline}{
%%     ~~Describe \code{skipline} here~~
}
  \item{gyou}{
%%     ~~Describe \code{gyou} here~~
}
  \item{comment.char}{
%%     ~~Describe \code{comment.char} here~~
}
  \item{table.property}{
%%     ~~Describe \code{table.property} here~~
}
  \item{tb}{
%%     ~~Describe \code{tb} here~~
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
  \item{gyou.margin}{
%%     ~~Describe \code{gyou.margin} here~~
}
  \item{target.line}{
%%     ~~Describe \code{target.line} here~~
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
function(findseq,skipline,gyou,comment.char="#",
			table.property,tb,outfile,h=TRUE,is.ss2=FALSE,gyou.margin=0,
                                target.line=NULL,...){
#  for(i in c(startpoint:(length(table.property)-1))){
#  if(table.property[i]!=0 && read.table(outfile,skip=i-1,nrow=1)[1]==findseq){

  if(is.null(target.line)){
#    tb <- read.table(outfile,fill=T,col.names=paste("V",1:max(table.property),sep=""),as.is=T,
#                     blank.lines.skip=FALSE)
    tmp <- 1:nrow(tb)
    target.line <- tmp[tb[,1]==findseq]
    if(length(target.line)==0) cat("Cannot find target line!!!")
  }
  if(is.null(gyou)) gyou <- count.next.line(target.line,skipline,table.property[1:nrow(tb)])-target.line-skipline-gyou.margin
  a <- read.table(outfile,skip=target.line+skipline,header=FALSE,
                  nrow=gyou,comment.char=comment.char,...)
                  
  if(h==TRUE) {
    a.name <- read.table(outfile,skip=target.line+skipline-1,header=FALSE,nrow=1,colClasses="character")#as.is=TRUE)
    dimnames(a) <- list(a[,1],as.character(a.name))
  }
  if(is.ss2==FALSE){
    a <- a[,-1]
  }
  list(a,target.line+gyou)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
