\name{getALK.ss2}
\alias{getALK.ss2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getALK.ss2(repfile = "ss2.rep", cl = NULL, tb = NULL, all = FALSE, qt = 4)
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
  \item{all}{
%%     ~~Describe \code{all} here~~
}
  \item{qt}{
%%     ~~Describe \code{qt} here~~
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
function(repfile="ss2.rep",cl=NULL,tb=NULL,#target.line=NULL,
                                    all=FALSE, qt=4){
  # 2008/4/1: modified to read all data by quarter with the option of (all=TRUE, and qt=4)
  
  vskipline <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),3,6)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
                                     table.property=cl,tb=tb,
                                     outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  ALK <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,tb=tb,
                              table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                              target.line=name.label[[2]])
  ALK[[2]] <- ALK[[2]]
#  }
#  else{
#    name.label <- find.and.read.table("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
#                                       table.property=cl,startpoint=target.line-50,
#                                       outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                      colClasses="character")  
#    ALK <- find.and.read.table("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
#                               startpoint=target.line-50)
#  }
  colnames(ALK[[1]]) <- as.character(name.label[[1]])  

  if(all==TRUE){
    tmp <- ALK[[1]]
    tmp.line <- ALK[[2]]-nrow(ALK[[1]])
    
    res <- list()
    res[[1]] <- tmp

    desc <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),"SEASON:","Seas:")
    desc.line <- which(tb[,1]==desc)
      
    for(i in 1:length(desc.line)){
#      tmp <- find.and.read.table(desc,skipline=1,gyou=NULL,
#                                 table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                 startpoint=tmp.line+1)
#      browser()
      tmp <- find.and.read.table2(desc,skipline=1,gyou=NULL,tb=tb,
                                 table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                                  target.line=desc.line[i])
      
      res[[i]] <- tmp[[1]]
      dimnames(res[[i]]) <- dimnames(res[[1]])
      tmp.line <- tmp[[2]]-nrow(ALK[[1]])
    }
    ALK[[1]] <- res
  }

  list(ALK[[1]],ALK[[2]])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
