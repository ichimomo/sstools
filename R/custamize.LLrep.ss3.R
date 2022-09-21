custamize.LLrep.ss3 <-
function(a,rm.zerolambda=FALSE,multilambda=TRUE){
  TLL <- as.numeric(as.character(a$TLL[c(2,5,6),2]))
  names(TLL) <- c("Total Likelihood","Survey total","Length_comp total")
  
  penalties <- as.numeric(as.character(a[[1]][tmp <- sort(c(4,3,7,9,11,12,8,10)),2]))
  names(penalties) <- as.character(a[[1]][tmp,1])

  a0 <- as.data.frame(t(a[[2]]))

  if(rm.zerolambda==FALSE){
    tmp <- (a0$"Length_like"!=0)|(a0$"SizeFreq_like"!=0)
  }
  else{
    tmp <-  a0$"Length_like"!=0  & a0$"Length_lambda"!=0
  }
  if(length(tmp)>0){
    if(multilambda==TRUE){
      length.LL <- as.numeric(as.character(a0$"Length_like"[tmp])) * as.numeric(as.character(a0$"Length_lambda"[tmp]))
    }
    else{
      length.LL <- as.numeric(as.character(a0$"Length_like"[tmp]))
    }
    names(length.LL) <- paste2("Len_F",rownames(a0))
  }
  else{
    xx <- a0[-1,substr(colnames(a0),1,13)=="SizeFreq_like"]
    xx <- as.data.frame.table(as.matrix(xx))
    xx[,3] <- as.nc(xx[,3])
    length.LL <- xx[!is.na(xx[,3]),3]
    names(length.LL) <- xx[!is.na(xx[,3]),2]
  }

  if(rm.zerolambda==FALSE){
    tmp <- a0$"Surv_like"!=0
  }
  else{
    tmp <- a0$"Surv_like"!=0  & a0$"Surv_lambda"!=0    
  }
 
  if(multilambda==TRUE){
    surv.LL <- as.numeric(as.character(a0$"Surv_like"[tmp]))*as.numeric(as.character(a0$"Surv_lambda"[tmp]))    
  }
  else{
    surv.LL <- as.numeric(as.character(a0$"Surv_like"[tmp]))
  }
  names(surv.LL) <- paste2("Surv_F",rownames(a0))

  #--- age lambda --   "Age_lambda"   (とりあえず削除)
  if(0){
    if(rm.zerolambda==FALSE){
      tmp <- TRUE # 2011.1.10. 設定の違う複数のrepfileを比較するさい、エラーがでるので、とりあえず。冗長なモード
    }
    else{
      tmp <- which(as.nc(a0$"Age_like")!=0  & a0$"Age_lambda"!=0)    
    }

    if(length(tmp)>0){
      if(multilambda==TRUE){
        age.LL <- as.numeric(as.character(a0$"Age_like"[tmp]))*as.numeric(as.character(a0$"Age_lambda"[tmp]))    
      }
      else{
        age.LL <- as.numeric(as.character(a0$"Age_like"[tmp]))
      }
      #    names(age.LL) <- paste2("Age_F",rownames(a0))
      names(age.LL) <- paste2("Age_like_",names(a0$"Age_like")[tmp])
    }
  }
#  else{
    age.LL <- NULL
#  }
  #-----

  c(TLL,penalties,length.LL,surv.LL,age.LL)
}
