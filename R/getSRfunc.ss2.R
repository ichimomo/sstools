getSRfunc.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
#  read.char <- "SPAWN_RECRUIT"
  read.char <- "S/Rcurve" 
#  line.tmp <- 6
  line.tmp <- -2  

  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2(read.char,skipline=line.tmp,gyou=1,
                                     table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  res <- find.and.read.table2(read.char,skipline=2+line.tmp,gyou=NULL,fill=T,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
#}

  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
  if(!is.ss3(repfile)) res[[1]]$"pred_recr" <- res[[1]]$"pred-recr"
#  browser()
  res
}
