\name{getNAA.ss2}
\alias{getNAA.ss2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getNAA.ss2(repfile, cl = NULL, tb = NULL, target.line = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{cl}{
%%     ~~Describe \code{cl} here~~
}
  \item{tb}{
%%     ~~Describe \code{tb} here~~
}
  \item{target.line}{
%%     ~~Describe \code{target.line} here~~
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
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "NUMBERS_AT_AGE" #
  line.tmp <- ifelse(is.ss3(repfile),0,1)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    naa <- find.and.read.table2(read.char,skipline=line.tmp+1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    naa <- find.and.read.table(read.char,skipline=line.tmp+1,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
#  naa[[1]] <- lapply(naa[[1]],as.character)
#  naa[[1]] <- lapply(naa[[1]],as.numeric)
  naa[[1]] <- as.data.frame(naa[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(naa[[1]]) <- as.character(name.label[[1]])
  if(more.ss3.11(repfile)){
    colnames(naa[[1]])[7] <- "Year"
    colnames(naa[[1]])[11] <- "Per"    
  }
  else{
    if(is.ss3(repfile)){
      colnames(naa[[1]])[7] <- "Year"
      colnames(naa[[1]])[10] <- "Per"
    }}
  if(more.ss3.11(repfile)) naa[[1]] <- naa[[1]][naa[[1]]$"Beg/Mid"=="B",]
  naa
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
