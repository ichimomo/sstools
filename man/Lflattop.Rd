\name{Lflattop}
\alias{Lflattop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Lflattop(len, beta1, beta2, beta3, beta4, beta5, beta6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{len}{
%%     ~~Describe \code{len} here~~
}
  \item{beta1}{
%%     ~~Describe \code{beta1} here~~
}
  \item{beta2}{
%%     ~~Describe \code{beta2} here~~
}
  \item{beta3}{
%%     ~~Describe \code{beta3} here~~
}
  \item{beta4}{
%%     ~~Describe \code{beta4} here~~
}
  \item{beta5}{
%%     ~~Describe \code{beta5} here~~
}
  \item{beta6}{
%%     ~~Describe \code{beta6} here~~
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
function(len,beta1,beta2,beta3,beta4,beta5,beta6){
  res <- rep(0,length(len))
  T1 <- min(len)+(1+exp(-beta1))^{-1}*(max(len)-min(len))
  T2 <- T1      +(1+exp(-beta2))^{-1}*(max(len)-T1)
  T3 <- (1+exp(-beta3))^{-1}
  T4 <- (1+exp(-beta4))^{-1}
  
  for(i in 1:length(len)){
    if(len[i]<T1){
      res[i] <- T3+((len[i]-min(len))/(T1-min(len)))^((1+beta5)*(1-T3))
    }
    else{
      if(len[i] <= T2){
        res[i] <- 1
      }
      else{
      res[i] <- 1+((len[i]-T2)/(max(len)-T2))^((1+beta6)*(T4-1))
      }}}
  list(res,T=c(T1,T2,T3,T4))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
