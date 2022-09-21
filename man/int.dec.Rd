\name{int.dec}
\alias{int.dec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
int.dec(cate, pre.cate = NA, interactive.mode = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cate}{
%%     ~~Describe \code{cate} here~~
}
  \item{pre.cate}{
%%     ~~Describe \code{pre.cate} here~~
}
  \item{interactive.mode}{
%%     ~~Describe \code{interactive.mode} here~~
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
function(cate,pre.cate=NA,interactive.mode=FALSE){
  cate1 <- sort(unique(cate))
  n <- length(cate1)
  if(length(pre.cate)==1){
      cate.res <- rep(0,n)
      }
      else{
      cate.res <- pre.cate
  }
  if(interactive.mode==TRUE){
    cat(as.character(cate1),"\n")
    for(i in 1:n){
      cat(as.character(cate1[i]),": ", as.character(pre.cate[i])," ")
      tmp <- readline()
      if(tmp!="n"&tmp!="AN"){
        cate.res[i] <- tmp
      }
      else{
        if(tmp=="AN")
          break
      }
    }
    cat("Do new category?")
    tmp <- readline()
  }
  else{
    tmp <- "Y"
    if(tmp=="Y"){
      new.dat <- rep(0,length(cate))
      for(i in 1:n){
        new.dat[as.character(cate)==as.character(cate1)[i]] <-
          as.character(cate.res[i])
      }
    }
    else{
      new.dat <- NA
    }
  }
  return(list(category.list=cbind(as.character(cate1),cate.res),newdat=new.dat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
