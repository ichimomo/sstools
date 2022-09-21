plotLL.ss <-
function(replist,repfile.legend=NULL,
                      is.plot=TRUE,multilambda=TRUE){
  rep.name <- character()
  nrep <- length(replist)
  for(i in 1:nrep){
    rep.name[i] <- chline(replist[i],n=35)
    tmp <- read.LLrep(replist[i])
    tmp <- custamize.LLrep.ss3(tmp,multilambda=multilambda)
    if(i==1){
      LLdata <- matrix(0,nrep,length(tmp))
      colnames(LLdata) <- names(tmp)
    }
    LLdata[i,] <- tmp
  }

  if(is.plot==TRUE){
    par.tmp <- par()
    nflame <- ifelse(nrep>5,4,5)
    layout(matrix(1:(nflame*2),nflame,2,byrow=T),
           width=c(3,1.7))
    nflame <- par()$mfcol[1]*par()$mfcol[2]
    par(las=1)
    s <- 1
    for(i in 1:ncol(LLdata)){
      if(!(all(LLdata[,i]==0)|all(is.na(LLdata[,i])))){
        if(s%%2==1){
          par(mar=c(4,15,1,1))
        }
        else{
          par(mar=c(4,0,1,1))
        }
        plot(LLdata[,i],
             x <- 1:nrep,type="n",yaxt="n",pch=20,
             xlab="",ylab="",ylim=c(0.5,max(x)+0.5))
        if(s%%nflame==0) mtext(side=3,outer=T,line=0.5,"@ Likelihood",adj=0.1)
        abline(h=x,col="gray")
        points(LLdata[,i],
               x <- 1:nrep,type="o",yaxt="n",pch=1,
               xlab="",ylab="",ylim=c(0.5,max(x)+0.5))

        if(s%%2==1){
          if(is.null(repfile.legend)){
            label.tmp <- rep.name
          }
          else{
            label.tmp <- repfile.legend
          }
          axis(at=x,labels=label.tmp,side=2)
        }
        title(colnames(LLdata)[i])
        s <- s+1
      }
    }
    par(par.tmp)
  }
  invisible(LLdata)
}
