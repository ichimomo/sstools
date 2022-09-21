\name{show.simpleres}
\alias{show.simpleres}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
show.simpleres(repfiles, sp.season = 4, is.refpoint = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfiles}{
%%     ~~Describe \code{repfiles} here~~
}
  \item{sp.season}{
%%     ~~Describe \code{sp.season} here~~
}
  \item{is.refpoint}{
%%     ~~Describe \code{is.refpoint} here~~
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
function(repfiles,sp.season=4,
                           is.refpoint=FALSE){
  a <- LL <- spr <- as.list(rep(0,length(repfiles)))
  for(i in 1:length(a)){
    a[[i]] <- getBabs.ss2(repfiles[i])[[1]]
    if(is.ss3(repfiles[i])){
      LL[[i]] <- custamize.LLrep.ss3(read.LLrep(repfiles[i]))
    }
    else{
      LL[[i]] <- custamize.LLrep(read.LLrep(repfiles[i]))      
    }
    spr[[i]] <- getSPR.ss2(repfiles[i])[[1]]
  }

  res2 <- matrix(0,length(LL),length(LL[[1]]))
  colnames(res2) <- names(LL[[1]])
  res <- data.frame(Rave=rep(0,length(a)),Rsigma=rep(0,length(a)),R0=rep(0,length(a)),
                    Rini=rep(0,length(a)),
                    SSBstart=rep(0,length(a)),SSB0=rep(0,length(a)),SSBstart.SSB0=rep(0,length(a)),
                    SSB.end=rep(0,length(a)),
                    SPRstart=rep(0,length(a)),SPR.end=rep(0,length(a)),
                    Eq.catch=rep(0,length(a)))

  for(i in 1:length(a)){
    end.year <- max(a[[i]]$year[a[[i]]$period=="TIME"])
    start.year <- min(a[[i]]$year[a[[i]]$period=="TIME"])    
    res$Rave[i] <- mean(a[[i]]$recruit[a[[i]]$period=="TIME" & a[[i]]$season==1])
#    res$Rsigma[i] <- sd(log(a[[i]]$recruit[a[[i]]$period=="TIME" & a[[i]]$season==1]))
    res$Rsigma[i] <- sqrt(var(log(a[[i]]$recruit[tmp <- a[[i]]$period=="TIME" & a[[i]]$season==1]))*
                          (sum(tmp))/(sum(tmp)-1))
    res$R0[i] <- a[[i]]$recruit[a[[i]]$period=="VIRG" & a[[i]]$season==1][1]
    res$Rini[i] <- a[[i]]$recruit[a[[i]]$period=="INIT" & a[[i]]$season==1][1]    
  
    res$SSB0[i] <- a[[i]]$Spawn[a[[i]]$period=="VIRG" & a[[i]]$season==sp.season][1]
    res$SSBstart[i] <- a[[i]]$Spawn[a[[i]]$year==start.year & a[[i]]$season==sp.season]
    res$SSBstart.SSB0[i] <- res$SSBstart[i]/res$SSB0[i]
    res$SSB.end[i] <- a[[i]]$Spawn[a[[i]]$year==end.year & a[[i]]$season==sp.season]
    
    res$SPRstart[i] <- spr[[i]]$SPR[spr[[i]]$Year==start.year]
    res$SPR.end[i] <- spr[[i]]$SPR[spr[[i]]$Year==end.year-1]

    if(!is.ss3(repfiles[i])){
      res$Eq.catch[i] <- sum(a[[i]][a[[i]]$period=="INIT" & a[[i]]$season==1,
                                    substr(colnames(a[[i]]),1,5)=="ret_c"])
    }
    else{
      res$Eq.catch[i] <- sum(a[[i]][a[[i]]$period=="INIT" & a[[i]]$season==1,
                 substr(colnames(a[[i]]),1,9)=="retain(B)"])      
    }
    res2[i,] <- LL[[i]]
  }

  res <- cbind(res,res2)
  
  rownames(res) <- repfiles

  if(is.refpoint==TRUE && is.ss3(repfiles[1])){
    f.opt <- numeric()
#    source("~/R/takeuchi_code/ss3util_20090619.r")
#    source("~/R/takeuchi_code/SS3ypr_20090619.r")
    for(i in 1:length(repfiles)){
      aaa <- testAll(repfiles[i])
      f.opt[i] <- 1/aaa$fmult[max(aaa$ypr)==aaa$ypr]
      if(i==1){
        YPR <- aaa$ypr
      }
      else{
        YPR <- cbind(YPR,aaa$ypr)
      }
    }
    res <- cbind(res,f.opt)
    names(res)[length(res)] <- "Fmax/Fcur"
    if(is.refpoint){
      colnames(YPR) <- repfiles
    }
    rownames(YPR) <- aaa$fmult
  }
  if(is.refpoint==TRUE){
    return(list(res,YPR))
  }
  else{
    return(res)    
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
