setncol <-
function(n){
  if(n<7){
    par(mfrow=c(3,2))
  }
  else{
    if(n<11){
      par(mfrow=c(ceiling(n/2),2))
    }
    else{
      par(mfrow=c(7,3))
      par(mar=c(2,2,1.5,0),oma=c(1,1,3,3),ps=12,mgp=c(1.8,0.3,0))
    }
  }
}
