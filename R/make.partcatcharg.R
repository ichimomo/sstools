make.partcatcharg <-
function(repfile=NULL,faa.res=NULL,yr.range=c(2002,2004),qt=4,gm=FALSE){

  as.numeric.terminalF <- function(x){
    terminal.F <- mean(x[,ncol(x)])
    res <- as.numeric(x)
    res[length(res)] <- terminal.F
    return(res)
  }

  if(is.null(faa.res)){
    a <- calFAA.ss2(repfile)
  }
  else{
    a <- faa.res
  }
#  tmp <- a$faa.array[as.numeric(rownames(a$faa))>=yr.range[1] &
#                     as.numeric(rownames(a$faa))<yr.range[2],,]
  tmp <- a$faa.array[floor(as.numeric(rownames(a$faa)))>=yr.range[1] &
                     floor(as.numeric(rownames(a$faa)))<=yr.range[2],,]  
  rownames(tmp) <- rep(1:qt,yr.range[2]-yr.range[1]+1)
  nfleet <- dim(tmp)[3]
  faa.yafq <- array(0,dim=c(dim(tmp)[1]/qt,dim(tmp)[2],dim(tmp)[3],qt)) #faa by year, age, fleet and quarter
  for(i in 1:qt){
    faa.yafq[,,,i] <- tmp[rownames(tmp)==i,,]  #=>最終的にはfaa.aqを作る
  }
  faa.yaq <- apply(faa.yafq,c(1,2,4),sum)
#  browser()
  if(gm==FALSE){
    faa.aq <- faa.aq.am <- apply(faa.yaq,c(2,3),mean)
  }else{
    faa.aq <- apply(faa.yaq,c(2,3),geomean)
    faa.aq <- faa.aq.gm <- ifelse(is.na(faa.aq),0,faa.aq)
  }

  faa.aq[nrow(faa.aq),qt] <- mean(faa.aq[nrow(faa.aq),])
  
  # calculate partial catch again
  faa.afq.tmp <- apply(faa.yafq,c(2:4),sum)
  faa.afq <- faa.afq.ratio <- array(0,dim=c(dim(tmp)[2],dim(tmp)[3],qt)) #faa by year, age, fleet and quarter
  for(j in 1:qt){
    for(i in 1:dim(faa.afq)[[1]]){
      if(i==dim(faa.afq)[[1]] & j==qt){
        # terminal ageのpartial Fの組成は、terminal ageの全四半期のものを使う
        x <- apply((faa.afq.tmp[i,,]/sum(faa.afq.tmp[i,,])),1,sum)
        faa.afq.ratio[i,,j] <- x/sum(x) 
      }
      else{
        faa.afq.ratio[i,,j] <- (faa.afq.tmp[i,,j]/sum(faa.afq.tmp[i,,j])) 
      }
      faa.afq[i,,j] <- faa.aq[i,j]*faa.afq.ratio[i,,j]                 
      faa.afq[i,,j] <- ifelse(is.nan(faa.afq[i,,j]),0,faa.afq[i,,j])              
    }}
  # aqの次元を1次元にする
  faa.qaf <- aperm(faa.afq,perm=c(3,1,2))
  faa.qaf.ratio <- aperm(faa.afq.ratio,perm=c(3,1,2))
#  browser()
  dim(faa.qaf) <-  c(dim(faa.qaf)[[1]]*dim(faa.qaf)[[2]],dim(faa.qaf)[[3]]) #<= tmp4
  dim(faa.qaf.ratio) <-  c(dim(faa.qaf.ratio)[[1]]*dim(faa.qaf.ratio)[[2]],dim(faa.qaf.ratio)[[3]])   #<= tmp3 （旧tmp3と少し違う、、）
  
  #2010.10.06 geometric meanのオプションを入れるため、大幅改訂==> ここから下は使わない
  if(0){
    tmp2 <- array(0,dim=c(qt,dim(tmp)[2],nfleet))
    for(i in 1:dim(tmp)[3]){
      tmp2[,,i] <- rowtapply(tmp[,,i])/(yr.range[2]-yr.range[1]+1) ## 年の次元を単純平均で減らす
    }
    tmp0 <- apply(tmp2,c(1,2),sum) # current F at age and season
    tmp3 <- tmp4 <- as.list(1:nfleet) # current F at age and season by fleet
    for(i in 1:nfleet){
      tmp3[[i]] <- tmp2[,,i]/tmp0
      tmp4[[i]] <- tmp2[,,i]
    }
    tmp3 <- as.data.frame(lapply(tmp3,as.numeric.terminalF)) # partila F by age (normazized)
    tmp4 <- as.data.frame(lapply(tmp4,as.numeric.terminalF)) # partila F by age (un-normalized)
    colnames(tmp3) <- colnames(tmp4) <-paste2("F",1:ncol(tmp4))
  }

  # weight at age
  tmp <- a$nma
  if(is.null(tmp$"age_Beg")){
    ages <- tmp$"Age_Beg"
  }  else{
    ages <- tmp$"age_Beg"
  }  
  waaf <- tmp[,substr(colnames(tmp),1,5)=="SelWt"][,1:nfleet]
  waaf <- waaf[order(ages),]
  rownames(waaf) <- rownames(faa.qaf) <- rownames(faa.qaf.ratio) <- sort(ages)

  waaf.mtrx<-as.matrix(waaf)
  return(list(ratio=faa.qaf.ratio,waaf=waaf,Fmat=faa.qaf,waaf.mtrx=waaf.mtrx))  # arguments for partial catch  )
}
