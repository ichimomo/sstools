Lflattop <-
function(len,beta1,beta2,beta3,beta4,beta5,beta6){
  res <- rep(0,length(len))
  T1 <- min(len)+(1+exp(-beta1))^{-1}*(max(len)-min(len))
  T2 <- T1      +(1+exp(-beta2))^{-1}*(max(len)-T1)
  T3 <- (1+exp(-beta3))^{-1}
  T4 <- (1+exp(-beta4))^{-1}
  
  for(i in 1:length(len)){
    if(len[i]<T1){
      res[i] <- T3+((len[i]-min(len))/(T1-min(len)))^((1+beta5)*(1-T3))
    }
    else{
      if(len[i] <= T2){
        res[i] <- 1
      }
      else{
      res[i] <- 1+((len[i]-T2)/(max(len)-T2))^((1+beta6)*(T4-1))
      }}}
  list(res,T=c(T1,T2,T3,T4))
}
