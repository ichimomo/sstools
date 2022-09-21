update.number <-
function(n1,n0,a,m,f){
  for(i in 2:(a-1)){
    n1[i] <- n0[i-1]*exp(-f[i-1]-m[i-1])
  }
  n1[a] <- n0[a-1]*exp(-f[a-1]-m[a-1]) + n0[a]*exp(-f[a]-m[a])
  n1
}
