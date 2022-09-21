getALK.ss <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,#target.line=NULL,
                                    all=FALSE){
  # 2008/4/1: modified to read all data by quarter with the option of (all=TRUE, and qt=4)
  
  vskipline <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),3,6)
  
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
#  if(is.null(target.line)){
  if(is.null(tb)){
    tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                     blank.lines.skip=FALSE)
  }
  name.label <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline-1,gyou=1,
                                     table.property=cl,tb=tb,
                                     outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
  ALK <- find.and.read.table2("AGE_LENGTH_KEY",skipline=vskipline,gyou=NULL,tb=tb,
                              table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE)
  ALK[[2]] <- ALK[[2]]

  colnames(ALK[[1]]) <- as.character(name.label[[1]])  
  
  if(all==TRUE){
    res <- list()
    res[[1]] <- ALK[[1]]

    desc <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile),"SEASON:","Seas:")
    desc.line <- which(tb[,1]==desc)

    for(i in 1:length(desc.line)){
      res[[i]] <- find.and.read.table2(desc,skipline=1,gyou=NULL,tb=tb,target.line=desc.line[i],
                                       table.property=cl,outfile=repfile,h=FALSE,is.ss2=TRUE,
                                       )[[1]]
      dimnames(res[[i]]) <- dimnames(ALK[[1]])
    }
    res[[i+1]] <- tb[desc.line,1:4]
    names(res) <- c(apply(tb[desc.line,1:4],1,paste,collapse=" "),"label")
    ALK[[1]] <- res
  }

  list(ALK[[1]],ALK[[2]])
}
