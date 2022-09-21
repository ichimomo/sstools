getWatL.ss2 <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "BIOLOGY" #
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
#    name.label <- find.and.read.table2(read.char,skipline=0,gyou=1,
#                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,
#    is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2(read.char,skipline=1,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
#    name.label <- find.and.read.table(read.char,skipline=0,startpoint=target.line-10,gyou=1,
#                                      table.property=cl,
#                                      outfile=repfile,h=FALSE,is.ss2=TRUE,
#                                      colClasses="character")  
    res <- find.and.read.table(read.char,skipline=1,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }

  res[[1]] <- as.data.frame(res[[1]])
  colnames(res[[1]]) <- c("bin","low","mean_size","Wt","mat_len","mat_Wt")
  res
}
