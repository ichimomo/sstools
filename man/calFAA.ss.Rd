\name{calFAA.ss}
\alias{calFAA.ss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calFAA.ss(repfile, datas = NULL, age.limit = NULL, namae = NULL, is.plot = F, nline = -1, Fmulti = 1, qt = 4, naa.target = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{datas}{
%%     ~~Describe \code{datas} here~~
}
  \item{age.limit}{
%%     ~~Describe \code{age.limit} here~~
}
  \item{namae}{
%%     ~~Describe \code{namae} here~~
}
  \item{is.plot}{
%%     ~~Describe \code{is.plot} here~~
}
  \item{nline}{
%%     ~~Describe \code{nline} here~~
}
  \item{Fmulti}{
%%     ~~Describe \code{Fmulti} here~~
}
  \item{qt}{
%%     ~~Describe \code{qt} here~~
}
  \item{naa.target}{
%%     ~~Describe \code{naa.target} here~~
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
function(repfile,datas=NULL,age.limit=NULL,namae=NULL,is.plot=F,
                       nline=-1,Fmulti=1,qt=4,naa.target=NULL){
  if(is.null(datas)){
    if(is.null(naa.target)){
      naa <- getNAA.ss2(repfile)
    }
    else{
      naa <- getNAA.ss2(repfile,target.line=naa.target)      
    }
    caa <- getCAA.ss2(repfile,target.line=naa[[2]]-10)
    nma <- getNMA.ss2(repfile,target.line=caa[[2]]-10,qt=qt)
    # In nma, M: natural mortality, Len_Mat: maturity rate, Wt_Beg: weight at age
    naa <- naa[[1]]
    caa <- list(caa=caa[[1]],caa.array=caa[[3]])
    nma <- nma[[1]]
  }
  else{
    naa <- datas$naa
    caa <- datas$caa
    nma <- datas$nma
  }

  naa$YQ <- as.numeric(naa$Year)+(as.numeric(naa$Seas)/qt)-1/qt
  totcatch <- apply(caa$caa.array,c(1,2),sum)

  if(is.null(datas)|is.null(datas$biom)){
    biom <- getBabs.ss2(repfile)[[1]]
  }
  else{
    biom <- datas$biom
  }

  is.catch <- substr(colnames(biom),1,9)=="ret_catch"
  if(sum(is.catch)==0) is.catch <- substr(colnames(biom),1,11)=="retain(B):_"
  if(sum(is.catch)==1){
    wtot.org <- biom[,is.catch]
  }
  else{
    wtot.org <- apply(biom[,is.catch],1,sum)  # total catch
  }
#  names(wtot.org) <- naa$YQ
  names(wtot.org) <- as.numeric(biom$year)+(as.numeric(biom$season)/qt)-1/qt

  faa <- faa.multi <- matrix(0,dim(totcatch)[[1]],dim(totcatch)[[2]],dimnames=dimnames(totcatch))
  faa.array <- faa.array.multi <-
    array(0,dim=dim(caa$caa.array),dimnames=dimnames(caa$caa.array))
  if(sum(colnames(nma)=="age")==0) nma$age <- nma$"Age"
  for(i in 1:nrow(faa)){
    for(j in 1:ncol(faa)){
      nage <- colnames(faa)[j]
      nyear <- rownames(faa)[i]
      if(totcatch[i,j]>0){
        faa[i,j] <- solv.Feq(cvec=totcatch[i,j],
                             nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                             mvec=nma$M[nma$age==nage]/qt)
        faa.array[i,j,] <- faa[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
        
        if(Fmulti!=1){
          faa.multi[i,j] <- solv.Feq(cvec=totcatch[i,j]*Fmulti,
                                     nvec=naa[naa$YQ==nyear,colnames(naa)==nage],
                                     mvec=nma$M[nma$age==nage]/qt)
          faa.array.multi[i,j,] <- faa.multi[i,j]*caa$caa.array[i,j,]/sum(caa$caa.array[i,j,])
        }
      }
    }
  }
  if(Fmulti==1){
    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,
                faa=faa,faa.array=faa.array,nma=nma,
                wtot=wtot.org,repfile=repfile)
  }
  else{
    dat <- list(naa=naa,caa=caa,caa.array=caa$caa.array,repfile=repfile,
                faa=faa.multi,faa.array=faa.array.multi,nma=nma,
                faa.org=faa,faa.array.org=faa.array,wtot=wtot.org)    
  }
  if(is.plot){
#    set.mypar()
    par(mfrow=c(2,1),mar=c(3,3,1,1))
    if(is.null(age.limit)) age.limit <- 1:ncol(faa)
    if(is.null(namae)) namae <- 1:ncol(faa)  
#    plotFvalue(list("F at age table"=faa),age.limit=age.limit,namae=namae,cex=0.7,VPA=FALSE,locate="n")
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(1952, 1959, 1960, 1969,
                  1970, 1979, 1980, 1989, 1990, 1999,2000,2006,2006,2008), 2, 7),VPA=FALSE)
    plotFvalue2(list("F at age table"=faa),
                year.limit=matrix(c(2002,2002.9,2003,2003.9,2004,2004.9,2005,2005.9,2006,2006.9,2007,2007.9), 2, 6),
                VPA=FALSE)    
  }
  return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
