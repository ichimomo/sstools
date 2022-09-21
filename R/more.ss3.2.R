more.ss3.2 <-
function(repfile){
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(a[1,1],3,6))>=3.2
  if(is.na(tmp)) tmp <- FALSE
  tmp
}
