\name{plot.nofishBio}
\alias{plot.nofishBio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.nofishBio(biom.mat, datfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{biom.mat}{
%%     ~~Describe \code{biom.mat} here~~
}
  \item{datfile}{
%%     ~~Describe \code{datfile} here~~
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
function(biom.mat,datfile=NULL){
  biom.mat <-  biom.mat[apply(is.na(biom.mat),1,sum)==0,]/1000
  plot(rownames(biom.mat),apply(biom.mat,1,max),type='n',xlab="Year",ylab="SSB(x1000)MT")
  yr <- as.numeric(rownames(biom.mat))
  n <- ncol(biom.mat)
  
  if(!is.null(datfile)){
    fleet.name <- read.fleetname(datfile)[1:(n-1)]
  }
  else{
    fleet.name <- paste2("F",1:(n-1))
  }

  colors <- rainbow(n)
  tmp <- cbind(0,biom.mat[,n],biom.mat[,1:(n-1)])
  for(i in 2:n){
    polygon(c(yr,rev(yr)),c(rep(tmp[,i-1]),rev(tmp[,i])),
            border="gray",col=colors[i],lwd=2)
  }
  legend("topright",max(biom.mat),fill=colors[2:n],legend=c("Current",fleet.name),ncol=2)
  invisible(biom.mat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
