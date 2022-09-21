plot.snail <-
function(repfile="ss2.rep",qt=4){
  faa <- calFAA.ss2(repfile,qt=qt,is.plot=F)
  tmp <- getSPR.ss(repfile)[[1]]
#  ssb <- tmp$"Bio_Smry"
  ssb <- tmp$"SPR"  
  names(ssb) <- tmp$Year
  res <- list(faa=apply(rowtapply(faa$faa),1,mean),ssb=ssb)
  x <- res$faa[match(names(res$ssb),names(res$faa))]
  y <- res$ssb[match(names(res$faa),names(res$ssb))]
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  years <- as.numeric(names(x))
  pch.tmp <- floor((years-1900)/10)
  pch.tmp <- pch.tmp-min(pch.tmp)+1
  plot(x,y,type="o",xlab="Average F",ylab="SPR/SPR0",pch=pch.tmp,lwd=2)
#       col=rainbow(9)[pch.tmp])  
}
