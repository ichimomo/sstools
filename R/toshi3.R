toshi3 <-
function(x){
  if(length(x)==1){
    x1 <- numeric()
    if(x<0) x1 <- x
      else{
          if(floor(log10(x))==0) x1 <- paste("0",x,sep="")
          if(floor(log10(x))==1) x1 <- x
        }
        x1
  }
  else{
    x1 <- rep(0,length(x))
    for(i in c(1:length(x1))){
        if(x[i]<0) x1[i] <- x[i]
           else{ 
              if(floor(log10(x[i]))==0) x1[i] <- paste("0",x[i],sep="")
              if(floor(log10(x[i]))==1) x1[i] <- x[i]
               }   
      }
        x1
  }
}

