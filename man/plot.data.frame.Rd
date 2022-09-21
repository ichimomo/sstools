\name{plot.data.frame}
\alias{plot.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.data.frame(data, datalist = NULL, name, title.name, nline, col.var, lty.var, lwd.var, ptype)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{datalist}{
%%     ~~Describe \code{datalist} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{title.name}{
%%     ~~Describe \code{title.name} here~~
}
  \item{nline}{
%%     ~~Describe \code{nline} here~~
}
  \item{col.var}{
%%     ~~Describe \code{col.var} here~~
}
  \item{lty.var}{
%%     ~~Describe \code{lty.var} here~~
}
  \item{lwd.var}{
%%     ~~Describe \code{lwd.var} here~~
}
  \item{ptype}{
%%     ~~Describe \code{ptype} here~~
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
function(data,datalist=NULL,name,title.name,nline,col.var,lty.var,lwd.var,ptype){
  x <- data$Year
  y <- data[[name]]
  plot(x[y!=0],y[y!=0],ylab=title.name,ylim=c(0,max(y,na.rm=T)),
       xlab="Year",type=ptype,lwd=lwd.var[1])
  title(title.name,line=nline)
  if(!is.null(datalist)){
    for(i in 1:length(datalist)){
      x1 <- datalist[[i]]$Year
      y1 <- datalist[[i]][[name]]
      points(x1[y1!=0],y1[y1!=0],col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type="l")
      }}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
