\name{plotFvalue2}
\alias{plotFvalue2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotFvalue2(vpadata, year.limit = matrix(c(1952, 1959, 1960, 1969, 1970, 1979, 1980, 1989, 1990, 1999, 2001, 2006), 2, 6), locate = "a", type = "b", tuika = FALSE, VPA = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vpadata}{
%%     ~~Describe \code{vpadata} here~~
}
  \item{year.limit}{
%%     ~~Describe \code{year.limit} here~~
}
  \item{locate}{
%%     ~~Describe \code{locate} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{tuika}{
%%     ~~Describe \code{tuika} here~~
}
  \item{VPA}{
%%     ~~Describe \code{VPA} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function(vpadata,
                        year.limit = matrix(c(1952, 1959,
                                 1960, 1969,
                                 1970, 1979,
                                 1980, 1989,
                                 1990, 1999,
                                 2001,2006),2,6),
                        locate="a",type="b",tuika=FALSE,VPA=TRUE,...
                          ){

  if(VPA==TRUE){
    temp <- vpadata$"F at age table"[,-ncol(vpadata$"F at age table")]
  }
  else{
    temp <- vpadata$"F at age table"
  }
  year.label <- as.numeric(rownames(temp))

#  if(range(year.limit) != range(year.label)){
#    stop("The dimension of 'year.limit' is invalid! Please input the vector with ", ncol(temp), " length.\n")
#  }  

  max.f.average <- 0
  leg.tmp <- FALSE
  for(i in c(1:ncol(year.limit))){
    if(sum(floor(year.label)>=year.limit[1,i] & floor(year.label)<=year.limit[2,i])!=0){
      leg.tmp[i] <- TRUE
      if(year.limit[1,i]==year.limit[2,i]){
        max.f.average <- max(c(max.f.average,
                              temp[floor(year.label) >= year.limit[1,i] &
                                                  floor(year.label) <= year.limit[2,i],]))
      }
      else{
        max.f.average <- max(c(max.f.average,
                              apply(as.matrix(temp[floor(year.label) >= year.limit[1,i] &
                                                  floor(year.label) <= year.limit[2,i],]),2,mean)))
      }
    }}
  
#  par(mar=c(4,4,1,1))
  if(tuika==FALSE){
    plot(1:ncol(temp),apply(temp,2,mean),type="n",xlab="Age",
       ylab="F",ylim=c(0,max.f.average),xaxt="n")
    abline(h=seq(from=0,to=3,by=0.5),col="gray")
  axis(1,at=1:ncol(temp),labels=colnames(temp))
  }

  for(i in c(1:ncol(year.limit))){
    temp2 <- temp[year.label >= year.limit[1,i] &
                                year.label <= year.limit[2,i],]
    if(!is.matrix(temp2)){
      points(c(1:ncol(temp)),
             temp2,pch=i,type=type,...)      
    }
    else{
      points(c(1:ncol(temp)),
           apply(as.matrix(temp2),2,mean)
           ,pch=i,type=type,...)
  }
  }

  if(locate!="n"){
                                        # 凡例
    if(locate=="a"){
      basyo <- c(ncol(temp),max.f.average*0.8)
    }
    else{
      if(is.vector(locate)){
        basyo <- locate
      }
      else{
        cat("Please click where you want to write the legend!\n")
        basyo <- locator(1)
      }}
#    browser()
    pch.tmp <- 1:ncol(year.limit)
    legend(basyo[1],basyo[2],legend=paste(year.limit[1,leg.tmp],
                               year.limit[2,leg.tmp],sep="-"),
           pch=pch.tmp[leg.tmp],#lty=c(1:nrow(year.limit)),
           bty="n",xjust=1,yjust=0.8)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
