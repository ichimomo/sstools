getBabs.ss2 <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,target.line=NULL){
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2("TIME_SERIES",skipline=0,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table2("TIME_SERIES",skipline=1,gyou=NULL,comment.char="",
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  else{
    name.label <- find.and.read.table("TIME_SERIES",skipline=0,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    res <- find.and.read.table("TIME_SERIES",skipline=1,startpoint=target.line-10,gyou=NULL,
                               table.property=cl,comment.char="",
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  colnames(res[[1]]) <- as.character(name.label[[1]])
  if(is.ss3(repfile)){
    tmp <- cbind(c("Yr","year"),
          c("Era","period"),
          c("Seas","season"),
          c("Bio_all","bio-all"),
          c("Bio_smry","bio-smry"),
          c("Recruit_0","recruit-0"))#,
#          c("enc(B):_1","enc_catch:_1"),
#          c("dead(B):_1","dead_catch:_1"),
#          c("retain(B):_1","ret_catch:_1"),
#          c("obs_cat:_1","obs_cat:_1"),
#          c("F:_1","Hrate-1"))
    colnames(res[[1]])[match(tmp[1,],colnames(res[[1]]))] <- tmp[2,]
    res[[1]]$"SpawnBio" <- as.numeric(as.character(res[[1]]$"SpawnBio"))    
  }
  res
}
