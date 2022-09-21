\name{replace.sd.offset}
\alias{replace.sd.offset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
replace.sd.offset(ctrfile, repfile, newctl = "control_new.ctl", def.sd = 0.2, replace.sd = c(T, T, T, T, T, T), vnumber = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ctrfile}{
%%     ~~Describe \code{ctrfile} here~~
}
  \item{repfile}{
%%     ~~Describe \code{repfile} here~~
}
  \item{newctl}{
%%     ~~Describe \code{newctl} here~~
}
  \item{def.sd}{
%%     ~~Describe \code{def.sd} here~~
}
  \item{replace.sd}{
%%     ~~Describe \code{replace.sd} here~~
}
  \item{vnumber}{
%%     ~~Describe \code{vnumber} here~~
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
function(ctrfile,repfile,newctl="control_new.ctl",def.sd=0.2,replace.sd=c(T,T,T,T,T,T),vnumber=1){
  b <-  getCPUE.ss2(repfile,target.line=11,vnumber=vnumber)[[3]]
  
  # $BCm!*!*(Bcontrol$B\%U\%!\%$\%k$NCV$-49$($O!"(Bctrl$B\%U\%!\%$\%k$N9T?t$r8e$m$+$i\%+\%&\%s\%H$7$F(B
  # $B9T$C$F$$$k$N$G!"(B"variance adjustment factors"$B$N5-=R0J9_$N\%U\%!\%$\%k$K$O!"(B
  # $B\%3\%a\%s\%H\%"\%&\%H0J30$NM>7W$J2~9T$rF~$l$J$$$h$&$K$7$F$/$@$5$$(B
  cl <- count.fields(ctrfile,comment="#",blank.lines.skip=T)
  a <- read.table(ctrfile,col.names=1:max(cl),fill=T)

  nline <- ifelse(vnumber<2,20,22)
#  a[nrow(a)-nline,1:length(def.sd)] <- b[,4]-def.sd
  a[nrow(a)-nline,1:length(def.sd)] <- b$r.m.s.e-def.sd  

  write.table(a,file=newctl,na="",row.names=F,col.names=F,quote=FALSE)

  return(b$r.m.s.e)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
