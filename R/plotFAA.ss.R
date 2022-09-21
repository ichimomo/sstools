plotFAA.ss <-
function(faa,col.var=1:100,lty.var=rep(1,100),lwd.var=rep(1,100),
                       ptype=rep("b",100),nline=-1,fleet.name=NA,
                       repfile.legend=NULL,by.age=TRUE){
  nage <- dim(faa[[1]]$faa.array)[[2]]
  nfleet <- dim(faa[[1]]$faa.array)[[3]]
  if(by.age)  setncol(nage) else setncol(nfleet)
  if(!by.age){
    for(i in 1:length(faa)){
      faa[[i]]$faa <- apply(faa[[i]]$faa.array,c(1,3),mean)
    }
  }

  #---------- legend -----
  nplot()
  if(is.null(repfile.legend)) repfile.legend <- sapply(faa,function(x) x[[8]])
  legend("topleft",legend=repfile.legend,col=col.var,lty=lty.var,lwd=lwd.var,ncol=2,bty="n")

  #---------- PLOT1 (by age)
  years <- as.numeric(rownames(faa[[1]]$faa))  
  for(i in 1:dim(faa[[1]]$faa)[[2]]){
    faa.mat <- sapply(faa,function(x) x$faa[,i])
    matplot(unique(floor(years)),rowtapply(faa.mat),ylab="",xlab="",
            col=col.var,lty=lty.var,lwd=lwd.var,type="l")
    if(by.age) title(paste("Age",dimnames(faa[[1]]$faa.array)[[2]][i]))
    else title(paste("Fleet ",dimnames(faa[[1]]$faa.array)[[3]][i]))

    if(i%%10==9){
      if(by.age)
        mtext(side=3,line=0.5,adj=0.1,"@ F at age by year",outer=T)
      else
        mtext(side=3,line=0.5,adj=0.1,"@ F by fleet (average through all ages) ",outer=T)      }
  }
}
