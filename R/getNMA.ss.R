getNMA.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL,qt=4){
  
  read.char <- ifelse(is.ss3(repfile),"Biology_at_age","Season")
  read.char <- ifelse(more.ss3.2(repfile),"Biology_at_age_in_endyr",read.char)  
  line.tmp <- ifelse(is.ss3(repfile),1,0)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=-1+line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    nma <- find.and.read.table2(read.char,skipline=0+line.tmp,gyou=NULL,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=-1+line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    nma <- find.and.read.table(read.char,skipline=0+line.tmp,startpoint=name.label[[2]]-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
#  nma[[1]] <- lapply(nma[[1]],as.character)
#  nma[[1]] <- lapply(nma[[1]],as.numeric)
  nma[[1]] <- as.data.frame(nma[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(nma[[1]]) <- as.character(name.label[[1]])

  if(qt>1 && !is.ss3(repfile)){
#    browser()
    tmp <- read.table(repfile,skip=nma[[2]]+1,nrow=nrow(nma[[1]])*(qt-1))
    colnames(tmp) <- colnames(nma[[1]])
    nma[[1]] <- rbind(nma[[1]],tmp)
  }
  nma
}
