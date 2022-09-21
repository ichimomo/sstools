lengthdist <-
function(agecomp){
  fleet.row <- sort(unique(agecomp[[1]]$fleet))  
  length.bin <- sort(unique(agecomp[[1]]$bin))
  sum.length <- array(0,dim=c(length(length.bin),length(fleet.row),2))
  dimnames(sum.length) <- list(length.bin,fleet.row,c("Obs","Exp"))

#  par(mfrow=c(5,5),mar=c(2,2,0.5,0.5),oma=c(0,0,3,3),mgp=c(2,1,0),ps=14)    
  s <- 1
  tmp <- paste(floor(as.numeric(agecomp[[1]]$year)),agecomp[[1]]$season,agecomp[[1]]$fleet,sep="-")
  tmp2 <- unique(tmp)
  all.length <- array(0,dim=c(length(length.bin),length(tmp2),2))
  all.label <- data.frame(label=rep(0,length(tmp2)),effN=rep(0,length(tmp2)),effN2=rep(0,length(tmp2)))
  tmp.ref <- paste(floor(as.numeric(agecomp[[2]]$Year)),agecomp[[2]]$Seas,agecomp[[2]]$Index,sep="-")

#  for(j in 1:length(fleet.row)){
    for(i in 1:length(tmp2)){
#      if(sum(agecomp[[1]]$fleet[tmp==tmp2[i]]==fleet.row[j])){
        x <- agecomp[[1]]$bin[tmp==tmp2[i]]
        y <- cbind(agecomp[[1]]$obs[tmp==tmp2[i]],agecomp[[1]]$exp[tmp==tmp2[i]])*
          agecomp[[2]]$Nsamp[tmp.ref==tmp2[i]]
        ## Summation of the data
        y <- y[!is.na(y[,1]),]
        x <- x[!is.na(x)]
        all.length[match(x,length.bin),s,1] <- obsp <- y[,1]/sum(y[,1])# all.length-->composition data
        all.length[match(x,length.bin),s,2] <- expp <- y[,2]/sum(y[,2])
        all.label$effN[s] <- sum(expp*(1-expp))/sum((obsp-expp)^2)
        all.label$effN2[s] <- sum(obsp*(1-obsp))/sum((obsp-expp)^2)        
        all.label$label[s] <- tmp2[i]
        fleet.tmp <- mean(agecomp[[1]]$fleet[tmp==tmp2[i]])
        sum.length[match(x,length.bin),fleet.tmp,] <- sum.length[match(x,length.bin),fleet.tmp,]+y
        s <- s+1                
      }
#}}
  invisible(list(all.length=all.length,label=all.label,sum.length=sum.length))
}
