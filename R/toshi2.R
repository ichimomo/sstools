toshi2 <-
function(x){  
  if(length(x)==1){
    x1 <- numeric()
    if(x<0) x1 <- x
      else{
          if(floor(log10(x))==0) x1 <- paste("000",x,sep="")
          if(floor(log10(x))==1) x1 <- paste("00",x,sep="")
          if(floor(log10(x))==2) x1 <- paste("0",x,sep="")          
          if(floor(log10(x))==3) x1 <- x
          if(x==0) x1 <- "0000"                        
        }
        x1
  }
  else{
    x1 <- rep(0,length(x))
    for(i in c(1:length(x1))){
        if(x[i]<0) x1[i] <- x[i]
           else{ 
              if(floor(log10(x[i]))==0) x1[i] <- paste("000",x[i],sep="")
              if(floor(log10(x[i]))==1) x1[i] <- paste("00",x[i],sep="")
              if(floor(log10(x[i]))==2) x1[i] <- paste("0",x[i],sep="")              
              if(floor(log10(x[i]))==3) x1[i] <- x[i]
              if(x[i]==0) x1[i] <- "0000"              
               }   
      }
        x1
  }
}

