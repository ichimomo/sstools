custamize.LLrep <-
function(a,rm.zerolambda=TRUE,multilambda=TRUE){
  TLL <- sum(a[[1]][,2])
  names(TLL) <- "TLL"
  
  penalties <- a[[1]][c(7:13),2]
  names(penalties) <- a[[1]][c(7:13),1]

  if(rm.zerolambda==FALSE){
    tmp <- a[[2]]$"length_like"!=0
  }
  else{
    tmp <- a[[2]]$"length_like"!=0  & a[[2]]$"length_lambda"!=0
  }
  if(multilambda==TRUE){  
    length.LL <- a[[2]]$"length_like"[tmp] * a[[2]]$"length_lambda"[tmp]
  }
  else{
    length.LL <- a[[2]]$"length_like"[tmp]    
  }
  names(length.LL) <- paste2("Len_F",a[[2]]$"Fleet"[tmp])

  if(rm.zerolambda==FALSE){
    tmp <- a[[2]]$"surv_like"!=0
  }
  else{
    tmp <- a[[2]]$"surv_like"!=0  & a[[2]]$"surv_lambda"!=0    
  }
  
  if(multilambda==TRUE){
#    browser()
    surv.LL <- a[[2]]$"surv_like"[tmp]*a[[2]]$"surv_lambda"[tmp]    
  }
  else{
    surv.LL <- a[[2]]$"surv_like"[tmp]
  }
  names(surv.LL) <- paste2("Surv_F",a[[2]]$"Fleet"[tmp])  
  
  c(TLL,penalties,length.LL,surv.LL)
}
