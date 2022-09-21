\name{to.quarterbase}
\alias{to.quarterbase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
to.quarterbase(outdata, qt = 4, age.specific.mat = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outdata}{
%%     ~~Describe \code{outdata} here~~
}
  \item{qt}{
%%     ~~Describe \code{qt} here~~
}
  \item{age.specific.mat}{
%%     ~~Describe \code{age.specific.mat} here~~
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
function(outdata,qt=4,age.specific.mat=TRUE){ 
  # bological parameter
  if(is.null(outdata$nma$"age_Beg")){
    ages <- outdata$nma$"Age_Beg"
  }
  else{
    ages <- outdata$nma$"age_Beg"
  }
  tmp <- order(ages)
  nmaa <- outdata$nma$M[tmp]  # F at age
  caa.array <- outdata$caa.array
  waa <- outdata$nma$"Wt_Beg"[tmp] # weight at age ( in calculating SSB, the model uses Wt_Beg)
  waa.mid <- outdata$nma$"Wt_Mid"[tmp] # another weight at age 2008/3/31
  if(age.specific.mat==TRUE){
    maa <- outdata$nma$"Age_Mat"[tmp]  # maturity at age if specific maturity at age
  }
  else{
    maa <- outdata$nma$"Len_Mat"[tmp]  # maturity at age if logistic maturity at age
  }
  ages <- sort(ages)
  names(maa) <- names(waa) <- names(nmaa) <- ages

  # estimated Numbers at age
  naat <- outdata$naa  # numbers at age estimated in VPA -> to covert quarter base
  measured.year <- range(as.numeric(naat$YQ[naat$Per=="TIME"]))
  qt.tmp <- naat$Seas
  rownames(naat) <- naat$YQ
#  naat <- naat[,-c(1:3,ncol(naat))]
  naat <- naat[,!is.na(as.numeric(colnames(naat)))]  
  naat <- naat*1000
  faat <- outdata$faa  # estimated fishing mortality

  #------- # total catch by year
  n.age <- length(nmaa)   # number of age
  
  # expand to quarter (naat,faat -> naat.qt, faat.qt)
  naat.qt <- matrix(0,nrow(naat),ncol(naat)*qt,
                               dimnames=list(rownames(naat),ages))
  faat.qt <- matrix(0,nrow(faat),ncol(faat)*qt,
                               dimnames=list(rownames(faat),ages))

  #expand to quarter (caa.array -> caa.array.qt)
  caa.array.qt <- wcaa.array.qt <-
    array(0,dim=c(dim(caa.array)[[1]],dim(caa.array)[[2]]*qt,dim(caa.array)[[3]]),
                        dimnames=list(dimnames(caa.array)[[1]],ages,dimnames(caa.array)[[3]]))  
  
  for(i in 1:nrow(naat)){
    for(a in 1:ncol(naat)){
      naat.qt[i,qt.tmp[i]+(a-1)*qt] <- naat[i,a]
    }}
  
  for(i in 1:nrow(faat)){
    for(a in 1:ncol(faat)){
      faat.qt[i,qt.tmp[i]+(a-1)*qt] <- faat[i,a]
    }
    faat.qt[i,ncol(faat.qt)] <- faat[i,a] # F in plus group
  }


  # expand caa to quarter
  for(f in 1:dim(caa.array)[[3]]){
    for(i in 1:dim(caa.array)[[1]]){
      for(a in 1:(dim(caa.array)[[2]]-1)){
        caa.array.qt[i,qt.tmp[i]+(a-1)*qt,f] <- caa.array[i,a,f]
      }
   }}

  # total catch (waa at begging)
  tc.beg <- tc.mid <- tc.alk <-
    matrix(0,dim(wcaa.array.qt)[[1]],dim(wcaa.array.qt)[[3]],
           dimnames=list(dimnames(wcaa.array.qt)[[1]],dimnames(wcaa.array.qt)[[3]]))
  for(i in 1:dim(wcaa.array.qt)[[3]]){
    wcaa.array.qt[,,i] <- sweep(caa.array.qt[,,i],2,waa,FUN="*")
    tc.beg[,i] <- as.matrix(apply(wcaa.array.qt[,,i],1,sum))
  }

  # total catch (waa at middle)
  for(i in 1:dim(wcaa.array.qt)[[3]]){
    tmp <- sweep(caa.array.qt[,,i],2,waa.mid,FUN="*")
    tc.mid[,i] <- as.matrix(apply(tmp,1,sum))
  }

  ## total catch from ALK
  #tc.alk <- calTotcatch.ALK(repfile)
  #tc.alk <- as.matrix(apply(tc.alk$wcaa.array,c(1,3),sum))

  outdata <- list("Numbers at age table"=naat.qt,
                  "F at age table"=faat.qt,
                  "Natural mortality at age"=nmaa,
                  "Weight at age (kg)"=waa,
                  "Maturity at age"=maa,
                  "Total catch"=t(as.matrix(t(outdata$wtot))),
                  "Age and year"=matrix(c(range(ages),
                    measured.year),2,2,byrow=TRUE),
                  tc.beg=tc.beg,
                  tc.mid=tc.mid,
#                  tc.alk=tc.alk,
                  waa.beg=waa,
                  waa.mid=waa.mid)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
