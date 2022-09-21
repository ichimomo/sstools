\name{read.std}
\alias{read.std}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.std(stdfile, foryear = NULL, esyear = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stdfile}{
%%     ~~Describe \code{stdfile} here~~
}
  \item{foryear}{
%%     ~~Describe \code{foryear} here~~
}
  \item{esyear}{
%%     ~~Describe \code{esyear} here~~
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
function(stdfile,foryear=NULL,esyear=NULL){
  b <- read.table(stdfile,fill=T)
  b <- b[-1,c(-1,-5)]
  colnames(b) <- c("label","estimates","std")
  b <- as.data.frame(b)
  b$estimates <- as.numeric(as.character(b$estimates))
  b$std <- as.numeric(as.character(b$std))

#  ssb <- fssb <- rec <- frec <- data.frame()

  tmp <- b$label=="spbio_std"

  if(is.null(esyear)) esyear <- 1:(sum(tmp)-2)
  ssb <- data.frame(x=esyear,
                    y=b$estimates[tmp][-1:-2],
                    sd=b$std[tmp][-1:-2])

  if(!is.null(foryear)){
    tmp <- b$label=="depletion"
    b0 <- b[tmp,][6:(6+foryear-1),]
    fssb <- data.frame(y=b0$estimates,
                      sd=b0$std,
                      x=(max(esyear)+1):(max(esyear)+foryear))
  }
  

  tmp <- b$label=="recr_std"
  rec <- data.frame(y=b$estimates[tmp][-1:-2],
                    x=esyear,
                    sd=b$std[tmp][-1:-2])

  if(!is.null(foryear)){
    tmp <- b$label=="depletion"
    b0 <- b[tmp,][(6+foryear):(6+foryear+foryear-1),]
    frec <- data.frame(y=b0$estimates,
                      sd=b0$std,
                      x=(max(esyear)+1):(max(esyear)+foryear))
  }

  if(is.null(foryear)){
    list(rec=rec,ssb=ssb)
  }
  else{
    list(rec=rec,frec=frec,ssb=ssb,fssb=fssb)    
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
