vnumber.ss3 <-
function(repfile){
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(a[1,1],5,8))
  if(is.na(tmp)) tmp <- FALSE
  if(more.ss3.2(repfile)==TRUE) tmp <- as.numeric(substr(a[1,1],3,6))
  tmp
}
