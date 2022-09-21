plotFvalue2 <-
function(vpadata,
                        year.limit = matrix(c(1952, 1959,
                                 1960, 1969,
                                 1970, 1979,
                                 1980, 1989,
                                 1990, 1999,
                                 2001,2006),2,6),
                        type="b",tuika=FALSE,VPA=TRUE,...
                          ){

  if(VPA==TRUE){
    temp <- vpadata$"F at age table"[,-ncol(vpadata$"F at age table")]
  }
  else{
    temp <- vpadata$"F at age table"
  }
  year.label <- as.numeric(rownames(temp))

  max.f.average <- 0
  leg.tmp <- FALSE
  for(i in c(1:ncol(year.limit))){
    if(sum(floor(year.label)>=year.limit[1,i] & floor(year.label)< year.limit[2,i])!=0){
      leg.tmp[i] <- TRUE
      if(year.limit[1,i]==year.limit[2,i]){
        max.f.average <- max(c(max.f.average,
                              temp[floor(year.label) >= year.limit[1,i] &
                                                  floor(year.label) <= year.limit[2,i],]))
      }
      else{
        max.f.average <- max(c(max.f.average,
                              apply(as.matrix(temp[floor(year.label) >= year.limit[1,i] &
                                                  floor(year.label) <= year.limit[2,i],]),2,mean)))
      }
    }}
  
#  par(mar=c(4,4,1,1))
  if(tuika==FALSE){
    plot(1:ncol(temp),apply(temp,2,mean),type="n",xlab="Age",
       ylab="F",ylim=c(0,max.f.average),xaxt="n")
#    abline(h=seq(from=0,to=3,by=0.5),col="gray")
  axis(1,at=1:ncol(temp),labels=colnames(temp))
  }

  for(i in c(1:ncol(year.limit))){
    temp2 <- temp[year.label >= year.limit[1,i] &
                                year.label <= year.limit[2,i],]
    if(!is.matrix(temp2)){
      points(c(1:ncol(temp)),
             temp2,pch=i,type=type,...)      
    }
    else{
      points(c(1:ncol(temp)),
           apply(as.matrix(temp2),2,mean)
           ,pch=i,type=type,...)
  }
  }

  
  pch.tmp <- 1:ncol(year.limit)
  legend("topright",legend=paste(year.limit[1,leg.tmp],
                     year.limit[2,leg.tmp],sep="-"),
         pch=pch.tmp[leg.tmp],#lty=c(1:nrow(year.limit)),
         bty="n",xjust=1,yjust=0.8)
}
