\name{read.datfile}
\alias{read.datfile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.datfile(datfile = "ss2.dat", outfile = "ss_new.dat", cpue.replace = NULL, catch.replace = NULL, length.replace = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datfile}{
%%     ~~Describe \code{datfile} here~~
}
  \item{outfile}{
%%     ~~Describe \code{outfile} here~~
}
  \item{cpue.replace}{
%%     ~~Describe \code{cpue.replace} here~~
}
  \item{catch.replace}{
%%     ~~Describe \code{catch.replace} here~~
}
  \item{length.replace}{
%%     ~~Describe \code{length.replace} here~~
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
function(datfile="ss2.dat",outfile="ss_new.dat",
                                             cpue.replace=NULL,catch.replace=NULL,length.replace=NULL){
  
  # aに欠損値を含む、全ての椒如尺タが入った行列を入れる。今後はaの中身を変更していく
  cf <- count.fields(datfile,comment="",blank.lines.skip=T)
  a <- read.table(datfile,fill=T,col.names=paste("V",1:max(cf),sep=""),as.is=T)  
  a <- as.data.frame(a)

  nyear <- as.numeric(a[2,1])-as.numeric(a[1,1])+1
  y1 <- nyear*as.numeric(a[3,1])+18
  ncpue <- as.numeric(a[y1,1])
  cpue <- a[cpue.range <- ((y1+1):(y1+ncpue)),]
  ndiscard <- as.numeric(a[ncpue+y1+2,1])
  nmeanwt <- as.numeric(a[ncpue+y1+3+ndiscard,1])
  nobs <- as.numeric(a[y2 <- ncpue+y1+10+ndiscard+nmeanwt,1])
  len.comp.obs <- a[length.range <- ((y2+1):(y2+nobs)),]
  catch <- a[catch.range <- 18:(y1-1),]
#  browser()

  # 後ろから順に置き換える
  if(!is.null(length.replace)){
    length.new <- matrix(NA,nrow(length.replace),ncol(length.replace))
    length.new[,1:ncol(length.replace)] <- as.matrix(length.replace)
    a <- rbind(a[1:(min(length.range)-1),],
               length.new,
               a[(max(length.range)+1):nrow(a),])
    a[y2,1] <- nrow(length.new)
  }

  if(!is.null(cpue.replace)){
    cpue.new <- matrix(NA,nrow(cpue.replace),ncol(cpue))
    cpue.new[,1:ncol(cpue.replace)] <- as.matrix(cpue.replace)
    a <- rbind(a[1:(min(cpue.range)-1),],
               cpue.new,
               a[(max(cpue.range)+1):nrow(a),])
    a[y1,1] <- nrow(cpue.new)
  }

  if(!is.null(catch.replace)){
    catch.new <- matrix(NA,nrow(catch.replace),ncol(catch))
    catch.new[,1:ncol(catch.replace)] <- as.matrix(catch.replace)
    a <- rbind(a[1:(min(catch.range)-1),],
               catch.new,
               a[(max(catch.range)+1):nrow(a),])
    a[17,1] <- nrow(catch.new)
    a[2,1] <- max(floor(as.numeric(rownames(catch.replace))))
  }

  cat("# This file is the dat file, in which some catch data were automatically replaced into zero\n",file=outfile,append=F)
  write.table(a,na="",file=outfile,row.names=F,col.names=F,append=T,quote=FALSE)
    
  # 古い椒宮ン椒悳ル椒宮イ椒困反靴靴ぼサ椒黠プ椒諚サ椒ぼズを返す
  invisible(list(catch=catch,size=len.comp.obs,cpue=cpue))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
