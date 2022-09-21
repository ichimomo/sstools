\name{custamize.LLrep}
\alias{custamize.LLrep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
custamize.LLrep(a, rm.zerolambda = TRUE, multilambda = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{rm.zerolambda}{
%%     ~~Describe \code{rm.zerolambda} here~~
}
  \item{multilambda}{
%%     ~~Describe \code{multilambda} here~~
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
function(a,rm.zerolambda=TRUE,multilambda=TRUE){
  TLL <- sum(a[[1]][,2])
  names(TLL) <- "TLL"
  
  penalties <- a[[1]][c(7:13),2]
  names(penalties) <- a[[1]][c(7:13),1]

  if(rm.zerolambda==FALSE){
    tmp <- a[[2]]$"length_like"!=0
  }
  else{
    tmp <- a[[2]]$"length_like"!=0  & a[[2]]$"length_lambda"!=0
  }
  if(multilambda==TRUE){  
    length.LL <- a[[2]]$"length_like"[tmp] * a[[2]]$"length_lambda"[tmp]
  }
  else{
    length.LL <- a[[2]]$"length_like"[tmp]    
  }
  names(length.LL) <- paste2("Len_F",a[[2]]$"Fleet"[tmp])

  if(rm.zerolambda==FALSE){
    tmp <- a[[2]]$"surv_like"!=0
  }
  else{
    tmp <- a[[2]]$"surv_like"!=0  & a[[2]]$"surv_lambda"!=0    
  }
  
  if(multilambda==TRUE){
#    browser()
    surv.LL <- a[[2]]$"surv_like"[tmp]*a[[2]]$"surv_lambda"[tmp]    
  }
  else{
    surv.LL <- a[[2]]$"surv_like"[tmp]
  }
  names(surv.LL) <- paste2("Surv_F",a[[2]]$"Fleet"[tmp])  
  
  c(TLL,penalties,length.LL,surv.LL)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
