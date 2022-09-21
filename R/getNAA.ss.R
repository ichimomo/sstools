getNAA.ss <-
function(repfile,cl=NULL,tb=NULL){#,target.line=NULL){
  read.char <- "NUMBERS_AT_AGE" #
  line.tmp <- ifelse(is.ss3(repfile),0,1)
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
  naa <- find.and.read.table2(read.char,skipline=line.tmp+1,gyou=NULL,
                              table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="")
#  }
#  else{
#    name.label <- find.and.read.table(read.char,skipline=line.tmp,startpoint=target.line-10,gyou=1,
#                                      table.property=cl,
#                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
#    naa <- find.and.read.table(read.char,skipline=line.tmp+1,startpoint=name.label[[2]]-10,gyou=NULL,
#                               table.property=cl,comment.char="",
#                               outfile=repfile,h=FALSE,is.ss2=TRUE)
#  }
#  naa[[1]] <- lapply(naa[[1]],as.character)
#  naa[[1]] <- lapply(naa[[1]],as.numeric)
  naa[[1]] <- as.data.frame(naa[[1]])
#  res[[1]][res[[1]]=="+1.#IND"] <- Inf
#  res[[1]] <- as.data.frame(res[[1]])
  colnames(naa[[1]]) <- as.character(name.label[[1]])
  if(more.ss3.11(repfile)){
    colnames(naa[[1]])[7] <- "Year"
    colnames(naa[[1]])[11] <- "Per"    
  }
  else{
    if(is.ss3(repfile)){
      colnames(naa[[1]])[7] <- "Year"
      colnames(naa[[1]])[10] <- "Per"
    }}
  if(more.ss3.11(repfile)) naa[[1]] <- naa[[1]][naa[[1]]$"Beg/Mid"=="B",]

  line <- naa[[2]]
  naa <- naa[[1]]
  age.cols <- !is.na(as.numeric(dimnames(naa)[[2]]))
  YQ <- as.numeric(naa$Year)+(as.numeric(naa$Seas)/4)-0.25
  tmp <- !is.na(YQ)
  naa.array <- array(0,dim=c(length(unique(YQ[tmp])),
                         sum(age.cols),length(unique(naa$BirthSeas))))
  
  s <- 1
  for(i in which(age.cols)){
    naa.array[,s,] <- tmp2 <- tapply(as.numeric(naa[tmp,i]),
                                    list(YQ[tmp],
                                         as.factor(unfactor(naa$BirthSeas[tmp]))),sum)
    s <- s+1
  }
  dimnames(naa.array) <- list(rownames(tmp2),colnames(naa)[age.cols],colnames(tmp2))      
  return(list(naa=naa,line=line,naa.array=naa.array))
}
