\name{plot.processgraph}
\alias{plot.processgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.processgraph(res, smp.lim = NULL, cpue.lim = NULL, dev.type = "pdf")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{smp.lim}{
%%     ~~Describe \code{smp.lim} here~~
}
  \item{cpue.lim}{
%%     ~~Describe \code{cpue.lim} here~~
}
  \item{dev.type}{
%%     ~~Describe \code{dev.type} here~~
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
function(res,smp.lim=NULL,cpue.lim=NULL,
                              dev.type="pdf"){
  # USAGE:
  # res <- doautorun()
  # plot.processgraph(res$nsmp.res,res$sd.cpue.mat,smp.lim=200,cpue.lim=c(0,1))
  nsmp.res <- res$nsmp.res
  sd.cpue.mat <- res$sd.cpue.mat
  LL <- res$LL
  ncalc <- (dim(nsmp.res)[[2]])

  if(dev.type=="pdf"){
    tmpfunc <- function(){
      if(names(dev.cur())!="null device") dev.off()
    }
    on.exit(tmpfunc())
    pdf(file="process_track.pdf",paper="a4",height=9)
  }

  # effective sample size
  set.mypar()  
  par(mfrow=c(3,1),mar=c(3,3,1,1))
#  b <- nsmp.res[,3:ncalc,4]
  b <- nsmp.res[,3:ncalc,2]
   b <- as.data.frame(b)
   boxplot(b,ylab="Effective sample size",ylim=smp.lim)

  # CPUE plot
  matplot(t(sd.cpue.mat),type="b",col=1,lty=1,xlab="N of calculation",
          ylab="SD of CPUE",ylim=cpue.lim)

  # -log LL plot
  plot(LL,type="b",xlab="N of calculation",ylab="Negative log LL")
  

   # effective sample size by fleets
  fn <- sort(unique(nsmp.res[,1,8]))
  par(mfrow=c(5,2),mar=c(3,3,2,1))
  for(i in fn){
#     boxplot(as.data.frame(nsmp.res[nsmp.res[,1,8]==i,3:ncalc,4]),
    boxplot(as.data.frame(nsmp.res[nsmp.res[,1,8]==i,3:ncalc,2]),    # 4列目だと、effective sample sizeになっていまう。2列目にして、実際に置き換えた値を入れることにする
             ylab=paste("ESS by fleet"),ylim=smp.lim)
     title(paste("fleet",i),line=1)
   }

  if(dev.type=="pdf"){
    dev.off()
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
