getSPR.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL){
  if(!is.ss3(repfile)){
    read.char <- "SPR_series"
    line.tmp <- 0
  }
  else{
    read.char <- "SPR_series_uses_R0="
    line.tmp <- 4
  }
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
  res <- find.and.read.table2(read.char,skipline=1+line.tmp,gyou=NULL,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
#  }
#  else{
#    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
#                                      table.property=cl,
#                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
#    res <- find.and.read.table(read.char,skipline=1+line.tmp,startpoint=name.label[[2]]-10,gyou=NULL,
#                               table.property=cl,comment.char="",
#                               outfile=repfile,h=FALSE,is.ss2=TRUE)
#  }
  res[[1]] <- lapply(res[[1]],as.character)
  res[[1]] <- lapply(res[[1]],as.numeric)
  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- as.character(name.label[[1]])
#  colnames(res[[1]])[2:which(colnames(res[[1]])=="Actual:")] <-
#    paste("R0-",colnames(res[[1]])[1:which(colnames(res[[1]])=="Actual:")],sep="")
#  browser()
  res
}
