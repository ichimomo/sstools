\name{getCPUE.ss2}
\alias{getCPUE.ss2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCPUE.ss2(repfile = "ss2.rep", cl = NULL, tb = NULL, target.line = NULL)
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
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){

  vnumber2 <- is.ss2.2(repfile)
  vnumber3 <- is.ss3(repfile)

  index1.char <- ifelse(!vnumber2&!vnumber3,"index","INDEX_1")
  vskipline <- ifelse(!vnumber2&!vnumber3,0,1)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  

  #  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  # read index_2
  res <- find.and.read.table2("INDEX_2",skipline=1,gyou=NULL,comment.char="",
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T)
  if(ncol(res[[1]])==10){
    colnames(res[[1]]) <- c("index","year","vuln_bio","obs","exp","eff_Q","SE","Dev","Like","Like+log(s)")
  }
  else{
    colnames(res[[1]]) <- c("index","year","Seas","vuln_bio","obs","exp","calc_Q","eff_Q","SE","Dev","Like","Like+log(s)")    
  }
  if(vnumber3){
    a <- strsplit(as.character(res[[1]]$index),"_")
    aa <- numeric()
    for(i in 1:length(a)){
      aa[i] <- a[[i]][1]
    }
#    res[[1]]$index <- as.numeric(t(as.matrix(as.data.frame(strsplit(as.character(res[[1]]$index),"_"))[1,])))
    res[[1]]$index <- as.numeric(aa)#as.numeric(t(as.matrix(as.data.frame()[1,])))      
  }
  #----- ad hoc solution to avoid the problems of "1.#QNAN"--------
  res[[1]]$exp <- as.numeric(as.character(res[[1]]$exp))
  #----------------------------------------------------------------  
  
  # read index_1  
  res[[3]] <- find.and.read.table(index1.char,skipline=vskipline,
                                  gyou=max(as.numeric(gsub("_","",substr(res[[1]][,1],1,2)))),
                                  startpoint=res[[2]]+3,
                                  comment.char="",
                                  table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)[[1]]      

  if(vnumber2){
    colnames(res[[3]]) <- c("Index","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype ","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","mean_(Input+extra)_SE","pen_mean_Qdev","rmse_Qdev")
  }
  else{
    if(vnumber3){
      colnames(res[[3]]) <- c("Fleet","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype","","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","Input+VarAdj","Input+VarAdj+extra","VarAdj","New_VarAdj","pen_mean_Qdev","rmse_Qdev")
    }
    else{
      colnames(res[[3]]) <- c("index","N","Nops","r.m.s.e.","mean_input_SE")         
    }}
  res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
