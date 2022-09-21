plot.effN <-
function(comps,col.var=1:length(comps)){

  par(mfrow=c(4,2),mar=c(3,3,2,2))
  ufleet <- sort(unique(comps[[1]]$fleet))

  x.list <- list()
  for(i in 1:length(comps)){
    comps[[i]] <- subset(comps[[i]],season>0)
    x.list[[i]] <- tapply(comps[[i]]$effN,list(comps[[i]]$fleet,floor(as.numeric(comps[[i]]$year)),comps[[i]]$season),mean)
  }

  for(j in 1:length(ufleet)){
    tmp <- lapply(x.list,function(x) x[j,,])
#    browser()
    matplot(range(as.numeric(rownames(tmp[[1]])[rowSums(!is.na(tmp[[1]]))>0])),
            c(0,max(sapply(tmp,max,na.rm=T),na.rm=T)),type="n",ylab="EffN",xlab="Years")
    for(k in 1:length(comps)){
      matpoints(rownames(tmp[[k]]),tmp[[k]],col=col.var[k],type="b")
    }
    title(paste("Fleet",j))

    if(j %% prod(par()$mfcol)==1)
      mtext(side=3,line=0.5,adj=0.1,paste("@ Effective sample size (number=quarter)"),outer=T)                
  }
}
