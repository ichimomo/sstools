\name{calTotcatch.ALK}
\alias{calTotcatch.ALK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calTotcatch.ALK(repfile, qt = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
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
function(repfile,qt=4){
  caa <- getCAA.ss2(repfile)
  WatL <- getWatL.ss2(repfile)[[1]]  #
  ALK <- getALK.ss2(repfile=repfile,all=TRUE,qt=qt)[[1]]

  label <- as.numeric(unfactor(ALK[[1]][,1]))
  for(i in 1:qt){  # delete the label of first line
    ALK[[i]] <- ALK[[i]][!is.na(label),-1]
  }
  label <- label[!is.na(label)]

  # sort the bin by increasing
  for(i in 1:qt){
    ALK[[i]] <- ALK[[i]][order(label),]
    rownames(ALK[[i]]) <- label[order(label)]
  }
  label <- label[order(label)]

  wcaa.array <- caa$caa.array
  dc <- dim(wcaa.array)
  # check the number of bin with that of WatL
  if(sum(WatL$low != label)==0){
    for(i in 1:dc[3]){
      for(j in 1:dc[2]){
        for(k in 1:dc[1]){
#          browser()
#          cat(qtback(dimnames(wcaa.array)[[1]][k])," ")
          wcaa.array[k,j,i] <- sum(caa$caa.array[k,j,i] * 
            ALK[[qtback(dimnames(wcaa.array)[[1]][k])]][,j] * # catch at age (in number) by each bin
               WatL$Wt) # weight by bin
    }}}
  }
  else{
    cat("Length of label and WatL is different !!!!")
  }

  return(list(wcaa.array=wcaa.array,ALK=ALK,WatL=WatL))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
