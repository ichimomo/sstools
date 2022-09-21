\name{plotSelect}
\alias{plotSelect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotSelect(selects, multiplot = FALSE, len.or.age = "Length", ptype = "l", col.var = 1, lty.var = 1, lwd.var = 1, nline = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selects}{
%%     ~~Describe \code{selects} here~~
}
  \item{multiplot}{
%%     ~~Describe \code{multiplot} here~~
}
  \item{len.or.age}{
%%     ~~Describe \code{len.or.age} here~~
}
  \item{ptype}{
%%     ~~Describe \code{ptype} here~~
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
  \item{nline}{
%%     ~~Describe \code{nline} here~~
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
function(selects,multiplot=FALSE,len.or.age="Length",ptype="l",col.var=1,lty.var=1,lwd.var=1,nline=1){
  # Only for allplot.ss2
  if(multiplot) len.rep <- length(selects)
  nfleet <- ncol(selects[[1]])
  setncol(nfleet)
  #  par(mfcol=c(ceiling(nfleet/2),2),ps=16,mar=c(4,4,1,1))
  # if the selectivity is competely same as the one plotted just before, the selectivity will not be plotted
  # This decision is done only in multiplot[[1]]
  old.selects <- 0
  title.tmp <- strsplit(colnames(selects[[1]]),"-")
  s <- 0
  ss <- numeric()
  for(i in 1:nfleet){
    selects0 <- selects[[1]]

    if(sum(old.selects!=selects0[,i])!=0 |
       ifelse(i==1,TRUE,title.tmp[[i]][1]!=title.tmp[[i-1]][1])){

      if(i>1){
        title(paste(title.tmp[[i-1]][1],"-",title.tmp[[ss[s]]][2],"to",title.tmp[[i-1]][2],"-",title.tmp[[i-1]][3]),line=nline)
      }
      
      plot(rownames(selects0),old.selects <- selects0[,i],type=ptype,xlab=len.or.age,ylab="Selectivity",lwd=lwd.var[1])
      s <- s+1
      ss[s] <- i

      if(multiplot){
        for(j in 2:len.rep){
          points(rownames(selects[[j]]),selects[[j]][,i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
        }}         

      nfile <- 1
      if(nfleet>10 && s\%\%10==0){
        mtext(side=3,line=0.5,adj=0.3,paste(len.or.age,"selectivity"),outer=T)
#        nfile <- nfile + 1
#        makedevice(filename="Select",dev.type=dev.type,filenum=nfile,htmlfile=htmlfile,
#                   new=T,append=T)
#        setncol(nfleet)
      }
    }}
  title(paste(title.tmp[[i-1]][1],"-",title.tmp[[ss[s]]][2],"to",title.tmp[[i-1]][2],"-",title.tmp[[i-1]][3]),line=nline)  
  mtext(side=3,line=0.5,adj=0.3,paste(len.or.age,"selectivity"),outer=T)  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
