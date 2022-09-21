plotSelect <-
function(selects,multiplot=FALSE,len.or.age="Length",ptype="l",col.var=1,lty.var=1,lwd.var=1,nline=1){
  # Only for allplot.ss2
  if(multiplot) len.rep <- length(selects)

  s <- 0
  # selectivityの推定数がすべてのrepfileで同じ場合
  if(var(unlist(lapply(selects,ncol)))==0 | is.na(var(unlist(lapply(selects,ncol))))){
    nfleet <- ncol(selects[[1]])
    setncol(nfleet)
    #  par(mfcol=c(ceiling(nfleet/2),2),ps=16,mar=c(4,4,1,1))
    # if the selectivity is competely same as the one plotted just before, the selectivity will not be plotted
    # This decision is done only in multiplot[[1]]
    old.selects <- 0
    title.tmp <- strsplit(colnames(selects[[1]]),"-")

    ss <- numeric()
    for(i in 1:nfleet){
      selects0 <- selects[[1]]
      if(sum(old.selects!=selects0[,i])!=0 |
         ifelse(i==1,TRUE,title.tmp[[i]][1]!=title.tmp[[i-1]][1])){

        if(i>1){
          title(paste(title.tmp[[i-1]][1],":",title.tmp[[ss[s]]][2],"-",title.tmp[[i-1]][2],":",title.tmp[[i-1]][3]),line=nline)
      }
        
        plot(rownames(selects0),old.selects <- selects0[,i],type=ptype,xlab="",ylab="",lwd=lwd.var[1])
        if(s %% prod(par()$mfcol)==1)
          mtext(side=3,line=0.5,adj=0.1,paste("@ ",len.or.age,"selectivity"),outer=T)            
        s <- s+1
        ss[s] <- i
        
        if(multiplot){
          for(j in 2:len.rep){
            points(rownames(selects[[j]]),selects[[j]][,i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }}         
      }}
    title(paste(title.tmp[[i-1]][1],":",title.tmp[[ss[s]]][2],"-",title.tmp[[i-1]][2],":",title.tmp[[i-1]][3]),line=nline)
  }
  else{ # selectivityの推定数が異なる場合。冗長だけどプロットはできるパターン
    cat("   !!warning number of selecctivity curves is different among repfiles.  Then redundant plot mode is used.\n")
    all.fleet <- sort(unique(unlist(lapply(selects,colnames))))
    nfleet <- length(all.fleet)
    setncol(nfleet)

    for(i in 1:nfleet){
      plot(range(as.numeric(rownames(selects[[1]]))),c(0,1),
           type="n",xlab="",ylab="",lwd=lwd.var[1])
      tmp <- all.fleet[i]==colnames(selects[[1]])
      title(all.fleet[i])
      if(sum(tmp)>0){
        points(rownames(selects[[1]]),selects[[1]][,tmp],
               type=ptype,xlab="",ylab="",lwd=lwd.var[1])}
      if(s %% prod(par()$mfcol)==1)
        mtext(side=3,line=0.5,adj=0.1,paste("@ ",len.or.age,"selectivity"),outer=T)            
      s <- s+1
      if(multiplot){
        for(j in 2:len.rep){
          tmp <- all.fleet[i]==colnames(selects[[j]])
          if(sum(tmp)>0){
            points(rownames(selects[[j]]),selects[[j]][,all.fleet[i]==colnames(selects[[j]])],
                   type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          }
        }}
  }}
  if(s < prod(par()$mfcol)) mtext(side=3,line=0.5,adj=0.1,paste("@ ",len.or.age,"selectivity"),outer=T)  }
