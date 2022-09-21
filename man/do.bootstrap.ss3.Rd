\name{do.bootstrap.ss3}
\alias{do.bootstrap.ss3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
do.bootstrap.ss3(namesfile = "Starter.SS", control.boot = "control_boot.SS", ss3.arg = "-nox -nohess", grad.criteria = 0.1, nboot = 300, is.plot = F, debug.mode = F, max.calc = 5, additive.num = 0, save.faa = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{namesfile}{
%%     ~~Describe \code{namesfile} here~~
}
  \item{control.boot}{
%%     ~~Describe \code{control.boot} here~~
}
  \item{ss3.arg}{
%%     ~~Describe \code{ss3.arg} here~~
}
  \item{grad.criteria}{
%%     ~~Describe \code{grad.criteria} here~~
}
  \item{nboot}{
%%     ~~Describe \code{nboot} here~~
}
  \item{is.plot}{
%%     ~~Describe \code{is.plot} here~~
}
  \item{debug.mode}{
%%     ~~Describe \code{debug.mode} here~~
}
  \item{max.calc}{
%%     ~~Describe \code{max.calc} here~~
}
  \item{additive.num}{
%%     ~~Describe \code{additive.num} here~~
}
  \item{save.faa}{
%%     ~~Describe \code{save.faa} here~~
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
function(namesfile="Starter.SS",control.boot="control_boot.SS",
                             ss3.arg="-nox -nohess",grad.criteria=0.1,nboot=300,
                         is.plot=F,debug.mode=F,max.calc=5,additive.num=0,save.faa=FALSE){

  # (1) starter.ss2$B$rFI$`(B
  # (2) $B#69TL\$NCM$r(Bnboot$B$KJQ99$7$F!"(Bstarter.ss2$B$r=q$-D>$9(B
  # (3) $BJQ99$5$l$?(Bstarter.ss2$B$,=q$-=P$5$l!"0l2s(BSS$B$,<B9T$5$l\%V!<\%H\%9\%H\%i\%C\%WMQ$N(Bdat$B\%U\%!\%$\%k$,$G$-$k(B
  # (4) $B\%3\%s\%H\%m!<\%k\%U\%!\%$\%k$,!"\%V!<\%H\%9\%H\%i\%C\%WMQ$KMQ0U$5$l$F$$$?$b$N(B(control.boot)$B$HCV$-49$o$k(B
  # (3) ($B0J2<!"\%V!<\%H\%9\%H\%i\%C\%W2sJ,$@$17+$jJV$9(B)
  #     SS2-nudata.dat$B$+$i0l8DJ,$N(Bdat$B\%U\%!\%$\%k$r@Z$j$@$7!"$=$N(Bdat$B\%U\%!\%$\%k(B(data-boot.ss)$B$r$b$H$K(B
  #     $B?dDj$7!"7k2L$r(Bss2boot-(i+additive.num).rep$B$K\%3\%T!<(B
  
#  if(.Platform$OS.type=="unix") convert.filename()

  file.copy2(from=namesfile,to=paste2(namesfile,"_o"))  

  #--- when the program is terminated, the namesfile (or starter file)---XS
  #                                  is replaced with the original one

  exit.function <- function(){
    file.copy2(from=namesfile             ,to=paste2(namesfile,"_last"))
    file.copy2(from=paste2(namesfile,"_o"),to=namesfile)
    file.copy2(from="ss3-org.par"         ,to="ss3.par")    
    save(grad.rec,file="grad.rec.R")
  }

  on.exit(exit.function())

  #---------- Read starter file
  names.obj <- names.obj.o <- read.table(namesfile,as.is=T)
  if(is.null(nboot)){
    nboot <- as.numeric(names.obj[11,1])  # Number of boot straps
  }
  if(nboot==0) stop("Please input a figure more than 0 in the 11th line")
  #---------- Write starter file
  names.obj[11,1] <- nboot
  names.obj[3,1] <- 0 # not use ss2.par
  write.table("#NuStarter.SS2",file=namesfile,row.names=F,col.names=F,quote=FALSE)      
  write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)      
  #----------
  
  grad.rec <- data.frame(grad=rep(0,nboot+1),ncalc=rep(0,nboot+1))  
  grad.tmp <- 10
  s <- 1

  #---------- Conduct 1 process for create dat files for bootstraps
  while(grad.tmp>grad.criteria && s<max.calc){ # $B7W;;$O#52s$^$G(B
    if(s>1){ # $B#22sL\$+$i$O(Bss.par$B$rFI$`(B
      names.obj[3,1] <- 1 
      write.table("#NuStarter.SS2",file=namesfile,row.names=F,col.names=F,quote=FALSE)      
      write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)            
    }
    if(debug.mode==F){
      doss3(ss3.arg=ss3.arg) #!!
    }
    grad.tmp <- read.grad(parfile="ss3.par") #!!
    s <- s+1
  }
  grad.rec$grad[1] <- grad.tmp
  grad.rec$ncalc[1] <- s-1
  
  file.copy2(from="Report.SSO",to="Report_org.SSO")  #!!
  file.copy2(from="ss3.par",to="ss3-org.par")  #!!
  file.copy2(from="Data.SS_New",to="Data.SS_New_org")  #!!
#  file.copy2(from=namesfile2,to=namesfile)
  
  cf <- count.fields("Data.SS_New",comment.char="")  #!!
  a <- read.table("Data.SS_New",fill=T,col.names=paste("V",1:max(cf),sep=""),comment.char="") #!!
  
  cut.point <- c(2,which(a[,1]==999))

  #---------- modify starter.ss for bootstrapping
  names.obj[1,1] <- "data-boot.ss"
  names.obj[2,1] <- control.boot
  names.obj[3,1] <- 1  
  names.obj[11,1] <- 0
  write.table("#NuStarter.SS2",file=namesfile,row.names=F,col.names=F,quote=FALSE)      
  write.table(names.obj,file=namesfile,row.names=F,col.names=F,quote=FALSE,append=T)
  #----------
  for(i in 1:nboot){
    write.table(a[(cut.point[i]+1):cut.point[i+1],],na="",
                file="data-boot.ss",                
                row.names=F,col.names=F,quote=FALSE)  # boot.dat$B$N99?7(B
    file.copy2(from="ss3-org.par",to="ss3.par")                        

    s <- 1
    grad.tmp <- 10
    while(grad.tmp>grad.criteria && s<max.calc){ # $B7W;;$O#52s$^$G(B
      if(debug.mode==F){
        doss3(ss3.arg=ss3.arg)
      }
      grad.tmp <- read.grad(parfile="ss3.par")
      s <- s+1      
    }
    grad.rec$grad[i+1] <- grad.tmp
    grad.rec$ncalc[i+1] <- s-1
      
    file.copy2(from="Report.sso",to=paste2("Report_b",toshi(i+additive.num),".sso"))
#    file.copy2(from="Forecast-report.SSO",to=paste2("Forecast-report_b",toshi(i+additive.num),".SSO"))
    file.copy2(from="data-boot.ss",to=paste("data-boot",toshi(i+additive.num),".ss",sep=""))
    file.copy2(from="CompReport.sso",to=paste("CompReport",toshi(i+additive.num),".sso",sep=""))    
    file.copy2(from="ss3.par",to=paste("ss3_b",toshi(i+additive.num),".par",sep=""))                                
  }

  ## plot of the results
  if(is.plot==T){
    tmp <- getBabs.ss2("ss3-org.rep")#,cl=cl0,tb=tb0)
    biom <- tmp[[1]]
    biom.target <- tmp[[2]]
    biom.list <- as.list(1:nboot)
    for(i in 1:nboot){
      biom.list[[i]] <- getBabs.ss2(paste2("Report",i+additive.num,".SSO"),
                                    target.line=biom.target-10-nrow(biom))[[1]]
    }
    plotBSR(biom,biom.list)
  }
  save(grad.rec,file="grad.rec.R")

  if(save.faa==TRUE){
    for(i in 1:nboot){
      faa <- calFAA.ss2(paste2("Report_b",toshi(i+additive.num),".SSO"))
      save(faa,file=paste2("faa",toshi(i+additive.num),".R"))
    }
  }
  return(grad.rec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
