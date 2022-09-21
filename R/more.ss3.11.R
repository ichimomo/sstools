more.ss3.11 <-
function(repfile){
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(a[1,1],5,8))>=3.11
  if(is.na(tmp)) tmp <- FALSE
  if(tmp==FALSE & more.ss3.2(repfile)==TRUE) tmp <- TRUE
  tmp
}
