\name{cor.dot}
\alias{cor.dot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cor.dot(cor.res, filename = "document/temp.dot", under.value = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cor.res}{
%%     ~~Describe \code{cor.res} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{under.value}{
%%     ~~Describe \code{under.value} here~~
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
function(cor.res, filename="document/temp.dot",under.value=0.5){
  #  $B\%0\%i\%U$N@_Dj(B
cat("digraph \"foodweb.dhp\" { \n size=\"7,7\"; \n rankdir=\"LR\";\n node [fontname=\"Helvetica\" fontsize=14 shape=ellipse]; \n edge [fontname=\"Helvetica\" fontsize=12]; \n center=1;\n ",file=filename)

#cor.name <- as.character(cor.res[,1])
#cor.res <- cor.res[,-1]
#dimnames(cor.res) <- list(1:58,1:58)
n <- dim(cor.res)[1]
cat(n)
#cat(dimnames(cor.res))
#write.table(gyakulogistic(cor.res))
for(i in c(1:n)){
  cat(colnames(cor.res)[i],"\n",file=filename,append=TRUE)
  for(j in c(1:n)){
        if(abs(cor.res[i,j])>under.value && i > j )
        cat(colnames(cor.res)[i],"->",
            colnames(cor.res)[j],
            "[label=",
            round(cor.res[i,j]),
            "]; \n",file=filename,append=TRUE)
      }}
cat("}",file=filename,append=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
