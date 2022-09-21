plotgrowth.ss <-
function(alk.mat,lwd.var=1,col.var=1,lty.var=1,add=FALSE,pch=1,...){
  browser()
  a <- as.matrix(alk.mat)
  x <- as.numeric(colnames(alk.mat)[-1])
  tmp <- a[,1]=="mean"
  tmp2 <- a[,1]=="sdsize"  
#  plot(x,as.numeric(a[tmp,-1]),type="b",xlab="Age",ylab="Length (cm)",lwd=lwd.var[1])
#  points(x,as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),type="l")
#  points(x,as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]),type="l")
  y <- cbind(as.numeric(a[tmp,-1]),
             as.numeric(a[tmp,-1])-as.numeric(a[tmp2,-1]),
             as.numeric(a[tmp,-1])+as.numeric(a[tmp2,-1]))

  if(add==FALSE){
    matplot(x,y,
            type=c("b","l","l"),xlab="Age",ylab="Length (cm)",lwd=lwd.var,col=col.var,lty=lty.var,
            pch=pch,...)
  }
  else{
    matpoints(x,y,
              type=c("b","l","l"),lwd=lwd.var,col=col.var,lty=lty.var,pch=pch)    
  }
  abline(h=seq(from=0,to=max(as.numeric(a[tmp,-1])),by=50),col="gray")
  invisible(list(x=x,y=y))
}
