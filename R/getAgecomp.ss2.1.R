getAgecomp.ss2.1 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                #target.line=NULL,
                                                len=TRUE){
  desc <- ifelse(len==TRUE,"FIT_LEN_COMPS","FIT_AGE_COMPS")
#  desc <- ifelse(len==TRUE,"FIT_SIZE_COMPS","FIT_AGE_COMPS")  
#  if(is.null(target.line)){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  res <- find.and.read.table2(desc,skipline=1,gyou=NULL,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
#}
#  else{
#    cl <- count.fields(repfile,blank.lines.skip=FALSE)
#    res <- find.and.read.table(desc,skipline=1,startpoint=target.line-50,gyou=NULL,
#                               table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)    
#  }
  if(len==TRUE){
    colnames(res[[1]]) <- c("Index","Year","Seas","Gender","Mkt","Nsamp","effN","Like")
  }
  else{
    colnames(res[[1]]) <- 
      c("Index","Year","Seas","Gender","Mkt","Ageerr","Lbin_lo","Lbin_hi","Nsamp","effN","Like")
  }
  res
}
