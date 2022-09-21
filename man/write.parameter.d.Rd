\name{write.parameter.d}
\alias{write.parameter.d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.parameter.d(zahyou, para.list, kiri = 1, moji = "-")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zahyou}{
%%     ~~Describe \code{zahyou} here~~
}
  \item{para.list}{
%%     ~~Describe \code{para.list} here~~
}
  \item{kiri}{
%%     ~~Describe \code{kiri} here~~
}
  \item{moji}{
%%     ~~Describe \code{moji} here~~
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
function(zahyou,para.list,kiri=1,moji="-"){
  n <- length(para.list)
  moji <- NULL
  for(i in c(1:n)) {
#    ifelse(length(para.list[[i]])==1,
    moji <- paste(moji, names(para.list)[i],": ",
                  paste.new(round(para.list[[i]][1:length(para.list[[i]])],kiri)),"\n")
  }
#           moji <- paste(moji, names(para.list)[i],": ",
#                         paste(round(min(para.list[[i]]),kiri)),moji,
#                         paste(round(max(para.list[[i]]),kiri)),"\n"))
#  }
  text(zahyou[1],zahyou[2],as.character(moji),pos=4)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
