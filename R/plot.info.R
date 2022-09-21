plot.info <-
function(replist){
  for(j in 1:length(replist)){
    a <- readLines(replist[j],n=81)
    a <- a[c(-9,-12:-60,-67)]
    a <- c(replist[j],getwd(),a)
    plot(1:length(a),type="n",ylab="",xlab="",axes=F)
    for(i in 1:length(a)){
      text(1,length(a)-i+1,chline(a[i],90),adj=c(0,1),cex=ifelse(i==1,1.3,1))
    }
  }
}
