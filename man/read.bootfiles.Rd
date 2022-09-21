\name{read.bootfiles}
\alias{read.bootfiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.bootfiles(bootfiles, calc.PS = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootfiles}{
%%     ~~Describe \code{bootfiles} here~~
}
  \item{calc.PS}{
%%     ~~Describe \code{calc.PS} here~~
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
function(bootfiles,calc.PS=FALSE){
  nboot <- length(bootfiles)
  biom <- list()
  for(i in 1:nboot){
      biom[[i]] <- getBabs.ss2(bootfiles[i])[[1]]
      cat(i," ")
    }

  biom.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    biom.mat[,i] <- biom[[i]]$SpawnBio
  }
  rownames(biom.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  biom.mat <- as.data.frame(t(biom.mat))
  
  rec.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    rec.mat[,i] <- biom[[i]]$"recruit-0"
  }
  rownames(rec.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  rec.mat <- as.data.frame(t(rec.mat))

  # total catch # previous version
#  tc.mat <- matrix(0,nrow(biom[[1]]),nboot)
#  for(i in 1:nboot){
#    tmp <- apply(a[,substr(colnames(biom[[i]]),1,9)=="ret_catch"],1,sum)  # total catch
#    tc.mat[,i] <- tapply(tmp,biom[[i]]$year,sum)
#  }
#  rownames(tc.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
#  tc.mat <- as.data.frame(t(tc.mat))

  # Calculate population size
  if(calc.PS==FALSE){
    population.size <- NULL
  }
  else{
#    population.size <- rec.mat
#    population.size[] <- 0
    for(i in 1:nboot){
      a <- getNAA.ss2(bootfiles[i])[[1]]
      b <- getNMA.ss2(bootfiles[i])[[1]]
      x <- apply(sweep(a[,-1:-10],2,b$Age_Mat[b$Seas==4],FUN="*"),1,sum)
      if(i==1){
        population.size <- matrix(0,nboot,length(x))
        dimnames(population.size) <- list(1:nboot,a$Year+a$Seas/4-0.25)
      }
      population.size[i,] <- x
    }
  }
  
  return(list(biom.list=biom,rec.mat=rec.mat,biom.mat=biom.mat,population.size=population.size))
  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
