\name{toshi2}
\alias{toshi2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
toshi2(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function(x){  
  if(length(x)==1){
    x1 <- numeric()
    if(x<0) x1 <- x
      else{
          if(floor(log10(x))==0) x1 <- paste("000",x,sep="")
          if(floor(log10(x))==1) x1 <- paste("00",x,sep="")
          if(floor(log10(x))==2) x1 <- paste("0",x,sep="")          
          if(floor(log10(x))==3) x1 <- x
          if(x==0) x1 <- "0000"                        
        }
        x1
  }
  else{
    x1 <- rep(0,length(x))
    for(i in c(1:length(x1))){
        if(x[i]<0) x1[i] <- x[i]
           else{ 
              if(floor(log10(x[i]))==0) x1[i] <- paste("000",x[i],sep="")
              if(floor(log10(x[i]))==1) x1[i] <- paste("00",x[i],sep="")
              if(floor(log10(x[i]))==2) x1[i] <- paste("0",x[i],sep="")              
              if(floor(log10(x[i]))==3) x1[i] <- x[i]
              if(x[i]==0) x1[i] <- "0000"              
               }   
      }
        x1
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
