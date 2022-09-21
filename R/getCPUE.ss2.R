getCPUE.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){

  vnumber2 <- is.ss2.2(repfile)
  vnumber3 <- is.ss3(repfile)

  index1.char <- ifelse(!vnumber2&!vnumber3,"index","INDEX_1")
  vskipline <- ifelse(!vnumber2&!vnumber3,0,1)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  

  #  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  # read index_2
  res <- find.and.read.table2("INDEX_2",skipline=1,gyou=NULL,comment.char="",
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T)
  if(ncol(res[[1]])==10){
    colnames(res[[1]]) <- c("index","year","vuln_bio","obs","exp","eff_Q","SE","Dev","Like","Like+log(s)")
  }
  else{
    colnames(res[[1]]) <- c("index","year","Seas","vuln_bio","obs","exp","calc_Q","eff_Q","SE","Dev","Like","Like+log(s)")    
  }
  if(vnumber3){
    a <- strsplit(as.character(res[[1]]$index),"_")
    aa <- numeric()
    for(i in 1:length(a)){
      aa[i] <- a[[i]][1]
    }
#    res[[1]]$index <- as.numeric(t(as.matrix(as.data.frame(strsplit(as.character(res[[1]]$index),"_"))[1,])))
    res[[1]]$index <- as.numeric(aa)#as.numeric(t(as.matrix(as.data.frame()[1,])))      
  }
  #----- ad hoc solution to avoid the problems of "1.#QNAN"--------
  res[[1]]$exp <- as.numeric(as.character(res[[1]]$exp))
  #----------------------------------------------------------------  
  
  # read index_1  
  res[[3]] <- find.and.read.table(index1.char,skipline=vskipline,
                                  gyou=max(as.numeric(gsub("_","",substr(res[[1]][,1],1,2)))),
                                  startpoint=res[[2]]+3,
                                  comment.char="",
                                  table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)[[1]]      

  if(vnumber2){
    colnames(res[[3]]) <- c("Index","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype ","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","mean_(Input+extra)_SE","pen_mean_Qdev","rmse_Qdev")
  }
  else{
    if(vnumber3){
      colnames(res[[3]]) <- c("Fleet","Do_Power","Power","Do_Env_var","Env_Link","Do_ExtraVar","Qtype","","Q","Num=0/Bio=1","Err_type","N","Npos","r.m.s.e.","mean_input_SE","Input+VarAdj","Input+VarAdj+extra","VarAdj","New_VarAdj","pen_mean_Qdev","rmse_Qdev")
    }
    else{
      colnames(res[[3]]) <- c("index","N","Nops","r.m.s.e.","mean_input_SE")         
    }}
  res
}
