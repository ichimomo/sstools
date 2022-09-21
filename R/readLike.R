readLike <-
function(filename){
  ## Read likelihood from multiple files (only for SS2)
  like.b <- matrix(0,13,n <- length(filename))
  for(i in 1:n){
    like.b[,i] <- read.table(filename[i],skip=10,nrow=13)[,2]
  }
  like.b
}
