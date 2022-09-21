is.ss2.2 <-
function(repfile){
  vnumber <- read.table(repfile,nrow=1,colClasses="character",header=F)
  tmp <- as.numeric(substr(vnumber[1],16,19))>=2
  if(is.na(tmp)) tmp <- FALSE
  tmp
}
