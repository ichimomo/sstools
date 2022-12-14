\name{make.partcatcharg}
\alias{make.partcatcharg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make.partcatcharg(repfile = NULL, faa.res = NULL, yr.range = c(2002, 2004), qt = 4, gm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{faa.res}{
%%     ~~Describe \code{faa.res} here~~
}
  \item{yr.range}{
%%     ~~Describe \code{yr.range} here~~
}
  \item{qt}{
%%     ~~Describe \code{qt} here~~
}
  \item{gm}{
%%     ~~Describe \code{gm} here~~
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
function(repfile=NULL,faa.res=NULL,yr.range=c(2002,2004),qt=4,gm=FALSE){

  as.numeric.terminalF <- function(x){
    terminal.F <- mean(x[,ncol(x)])
    res <- as.numeric(x)
    res[length(res)] <- terminal.F
    return(res)
  }

  if(is.null(faa.res)){
    a <- calFAA.ss2(repfile)
  }
  else{
    a <- faa.res
  }
#  tmp <- a$faa.array[as.numeric(rownames(a$faa))>=yr.range[1] &
#                     as.numeric(rownames(a$faa))<yr.range[2],,]
  tmp <- a$faa.array[floor(as.numeric(rownames(a$faa)))>=yr.range[1] &
                     floor(as.numeric(rownames(a$faa)))<=yr.range[2],,]  
  rownames(tmp) <- rep(1:qt,yr.range[2]-yr.range[1]+1)
  nfleet <- dim(tmp)[3]
  faa.yafq <- array(0,dim=c(dim(tmp)[1]/qt,dim(tmp)[2],dim(tmp)[3],qt)) #faa by year, age, fleet and quarter
  for(i in 1:qt){
    faa.yafq[,,,i] <- tmp[rownames(tmp)==i,,]  #=>$B:G=*E*$K$O(Bfaa.aq$B$r:n$k(B
  }
  faa.yaq <- apply(faa.yafq,c(1,2,4),sum)
#  browser()
  if(gm==FALSE){
    faa.aq <- faa.aq.am <- apply(faa.yaq,c(2,3),mean)
  }else{
    faa.aq <- apply(faa.yaq,c(2,3),geomean)
    faa.aq <- faa.aq.gm <- ifelse(is.na(faa.aq),0,faa.aq)
  }

  faa.aq[nrow(faa.aq),qt] <- mean(faa.aq[nrow(faa.aq),])
  
  # calculate partial catch again
  faa.afq.tmp <- apply(faa.yafq,c(2:4),sum)
  faa.afq <- faa.afq.ratio <- array(0,dim=c(dim(tmp)[2],dim(tmp)[3],qt)) #faa by year, age, fleet and quarter
  for(j in 1:qt){
    for(i in 1:dim(faa.afq)[[1]]){
      if(i==dim(faa.afq)[[1]] & j==qt){
        # terminal age$B$N(Bpartial F$B$NAH@.$O!"(Bterminal age$B$NA4;MH>4|$N$b$N$r;H$&(B
        x <- apply((faa.afq.tmp[i,,]/sum(faa.afq.tmp[i,,])),1,sum)
        faa.afq.ratio[i,,j] <- x/sum(x) 
      }
      else{
        faa.afq.ratio[i,,j] <- (faa.afq.tmp[i,,j]/sum(faa.afq.tmp[i,,j])) 
      }
      faa.afq[i,,j] <- faa.aq[i,j]*faa.afq.ratio[i,,j]                 
      faa.afq[i,,j] <- ifelse(is.nan(faa.afq[i,,j]),0,faa.afq[i,,j])              
    }}
  # aq$B$N<!85$r(B1$B<!85$K$9$k(B
  faa.qaf <- aperm(faa.afq,perm=c(3,1,2))
  faa.qaf.ratio <- aperm(faa.afq.ratio,perm=c(3,1,2))
#  browser()
  dim(faa.qaf) <-  c(dim(faa.qaf)[[1]]*dim(faa.qaf)[[2]],dim(faa.qaf)[[3]]) #<= tmp4
  dim(faa.qaf.ratio) <-  c(dim(faa.qaf.ratio)[[1]]*dim(faa.qaf.ratio)[[2]],dim(faa.qaf.ratio)[[3]])   #<= tmp3 $B!J5l(Btmp3$B$H>/$70c$&!"!"!K(B
  
  #2010.10.06 geometric mean$B$N\%*\%W\%7\%g\%s$rF~$l$k$?$a!"BgI}2~D{(B==> $B$3$3$+$i2<$O;H$o$J$$(B
  if(0){
    tmp2 <- array(0,dim=c(qt,dim(tmp)[2],nfleet))
    for(i in 1:dim(tmp)[3]){
      tmp2[,,i] <- rowtapply(tmp[,,i])/(yr.range[2]-yr.range[1]+1) ## $BG/$N<!85$rC1=cJ?6Q$G8:$i$9(B
    }
    tmp0 <- apply(tmp2,c(1,2),sum) # current F at age and season
    tmp3 <- tmp4 <- as.list(1:nfleet) # current F at age and season by fleet
    for(i in 1:nfleet){
      tmp3[[i]] <- tmp2[,,i]/tmp0
      tmp4[[i]] <- tmp2[,,i]
    }
    tmp3 <- as.data.frame(lapply(tmp3,as.numeric.terminalF)) # partila F by age (normazized)
    tmp4 <- as.data.frame(lapply(tmp4,as.numeric.terminalF)) # partila F by age (un-normalized)
    colnames(tmp3) <- colnames(tmp4) <-paste2("F",1:ncol(tmp4))
  }

  # weight at age
  tmp <- a$nma
  if(is.null(tmp$"age_Beg")){
    ages <- tmp$"Age_Beg"
  }  else{
    ages <- tmp$"age_Beg"
  }  
  waaf <- tmp[,substr(colnames(tmp),1,5)=="SelWt"][,1:nfleet]
  waaf <- waaf[order(ages),]
  rownames(waaf) <- rownames(faa.qaf) <- rownames(faa.qaf.ratio) <- sort(ages)

#  list(ratio=tmp3,waaf=waaf,Fmat=tmp4)  # arguments for partial catch
  list(ratio=faa.qaf.ratio,waaf=waaf,Fmat=faa.qaf)  # arguments for partial catch  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
