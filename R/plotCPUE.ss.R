plotCPUE.ss <-
function(cpue,col.var=1:100,lty.var=rep(1,100),lwd.var=rep(1,100),
                        ptype=rep("b",100),nline=-1,fleet.name=NA){
  fleetn <- unique(cpue[[1]]$index)  
  nfile <- 1
  multiplot <- length(cpue)>1
  
#  setncol(length(fleetn))
#  browser()
  par(mfcol=c(6,2))
  for(i in fleetn){
    #---------- observed and expected --------------
    cpue0 <- cpue[[1]]
    par(mar=c(0,3,2,1))
    cpue0$upper <- exp(log(cpue0$obs)+cpue0$SE*1.96)
    cpue0$lower <- exp(log(cpue0$obs)-cpue0$SE*1.96)
    
    matplot(x <- cpue0$year[cpue0$index==i],
            y <- cbind(cpue0$obs[cpue0$index==i],
                       cpue0$exp[cpue0$index==i],
                       cpue0$upper[cpue0$index==i],
                       cpue0$lower[cpue0$index==i]),
            pch=c(1,NA),col=c(1,1,1,1),type=c("b","l","l","l"),
            xlab="",ylab="CPUE",lty=c(1,1,2,2),lwd=lwd.var[1],xaxt="n",
            ylim=c(0,max(y)))
#    browser()
    title(paste("Fleet",i,":",ifelse(is.na(fleet.name[i]),"",fleet.name[i])),
          line=0.5,adj=0.1,font.main=1)        
    if(multiplot){
      for(j in 2:length(cpue)){
        cpue0 <- cpue[[j]]
        points(cpue0$year[cpue0$index==i],
               cpue0$exp[cpue0$index==i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
      }}

    #---------- Residuals  --------------
    cpue0 <- cpue[[1]]
    par(mar=c(2.5,3,0,1))
    plot(x <- cpue0$year[cpue0$index==i],
         y <- cpue0$Dev[cpue0$index==i],
         pch=c(1),col=c(1),type=c("l"),
         xlab="",ylab="Log residual",lty=c(1,1),lwd=lwd.var[1])
#    legend("topleft",paste("Residuals"),bty="n")            
#    title(paste("Fleet",i,":",fleet.name[i]),line=nline)
    if(multiplot){
      for(j in 2:length(cpue)){
        cpue0 <- cpue[[j]]
        points(cpue0$year[cpue0$index==i],
               cpue0$Dev[cpue0$index==i],type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
      }}
    abline(h=0,col="gray")
    
    #--------- Titles ----------------
    if(i%%16==0){
      mtext(side=3,line=nline,adj=0.1,"@ CPUE (Upper panel: expected (line) and observed (line+circle), Lower panel: Residuals (obs-exp))",outer=T)
    }
  }
  if(length(fleetn)<8){
    mtext(side=3,line=nline,adj=0.1,"@ CPUE (Upper panel: expected (line) and observed (line+circle), Lower panel: Residuals (obs-exp))",outer=T)
  }  
}
