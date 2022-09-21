\name{getSelect.ss2}
\alias{getSelect.ss2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getSelect.ss2(repfile = "ss2.rep", cl = NULL, tb = NULL, target.line = NULL, len = TRUE)
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
  \item{len}{
%%     ~~Describe \code{len} here~~
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
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL,len=TRUE){
  if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
    vskipline <- 5
  }
  else{
    vskipline <- 0
  }

  if(len==TRUE){
    desc <- "LEN_SELEX"
  }
  else{
    desc <- "AGE_SELEX"
    if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
      vskipline <- 3
    }
  }
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(desc,skipline=0+vskipline,gyou=1,
                                       table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
    
    res <- find.and.read.table2(desc,skipline=1+vskipline,gyou=NULL,
                                table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  else{
#    tmp <- count.fields(repfile,blank.lines.skip=FALSE)
    name.label <- find.and.read.table(desc,skipline=0+vskipline,startpoint=target.line-10,gyou=1,
                                      table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)
    
    res <- find.and.read.table(desc,skipline=1+vskipline,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
  }
  colnames(res[[1]]) <- unlist(lapply(name.label[[1]],as.character))#name.label[[1]]
#  rownames(res[[1]]) <- paste("F",res[[1]][,1],"-Y",res[[1]][,2],"-G",res[[1]][,3],sep="")
  if(is.ss3(repfile) && vnumber.ss3(repfile)>=3.03){
    if(len==FALSE){
      # Remove ?? lines at AGE_SELEX such as 'Asel2' and 'sel*wt, sel*ret*wt etc..',
      #    those lines are probably for the use of future projections
      res[[1]] <- res[[1]][substr(res[[1]]$factor,1,5)=="Asel",]
      # NOT include the column of 'Factor' for the function of 'calTotcatch.select' (2009/07/27), which might cuase error for other functions
      rownames(res[[1]]) <- paste("F",res[[1]]$fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      tmp <- -1:-7            
    }
    else{
#      rownames(res[[1]]) <- paste(res[[1]]$Factor,"-F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      res[[1]] <- res[[1]][substr(res[[1]]$Factor,1,4)=="Lsel",]      
      rownames(res[[1]]) <- paste("F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")
      tmp <- -1:-5            
    }
  }
  else{
    rownames(res[[1]]) <- paste("F",res[[1]][,1],"-Y",res[[1]][,2],"-G",res[[1]][,3],sep="")    
#    rownames(res[[1]]) <- paste("F",res[[1]]$Fleet,"-Y",res[[1]]$year,"-G",res[[1]]$gender,sep="")    
    tmp <- -1:-4    
  }
  list(as.data.frame(t(res[[1]][,tmp])),res[[2]])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
