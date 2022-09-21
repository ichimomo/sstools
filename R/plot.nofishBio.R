plot.nofishBio <-
function(biom.mat,datfile=NULL){
  biom.mat <-  biom.mat[apply(is.na(biom.mat),1,sum)==0,]/1000
  plot(rownames(biom.mat),apply(biom.mat,1,max),type='n',xlab="Years",ylab="SSB(x1000)MT")
  yr <- as.numeric(rownames(biom.mat))
  n <- ncol(biom.mat)
  
  if(!is.null(datfile)){
    fleet.name <- read.fleetname(datfile)[1:(n-1)]
  }
  else{
    fleet.name <- paste2("F",1:(n-1))
  }

  colors <- rainbow(n)
  tmp <- cbind(0,biom.mat[,n],biom.mat[,1:(n-1)])
  for(i in 2:n){
    polygon(c(yr,rev(yr)),c(rep(tmp[,i-1]),rev(tmp[,i])),
            border="gray",col=colors[i],lwd=2)
  }
  legend("topright",max(biom.mat),fill=colors[2:n],legend=c("Current",fleet.name),ncol=2)
  invisible(biom.mat)
}
