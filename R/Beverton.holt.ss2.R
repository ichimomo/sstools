Beverton.holt.ss2 <-
function(SSB,coef,B0=60000){
  4*coef[1]*coef[2]*SSB/(B0*(1-coef[1])+SSB*(5*coef[1]-1))
}
