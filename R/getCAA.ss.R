getCAA.ss <-
function(repfile,cl=NULL,tb=NULL,target.line=NULL){
  read.char <- "CATCH_AT_AGE"
  line.tmp <- ifelse(is.ss3(repfile),-1,0)
  if(is.null(cl)){
    cl <- count.fields(repfile,blank.lines.skip=FALSE)
  }  
  if(is.null(target.line)){
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    name.label <- find.and.read.table2(read.char,skipline=1+line.tmp,gyou=1,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    caa <- find.and.read.table2(read.char,skipline=abs(line.tmp),gyou=NULL,fill=T,
                      table.property=cl,tb=tb,outfile=repfile,h=FALSE,is.ss2=TRUE,comment.char="",colClasses="character")
  }
  else{
    name.label <- find.and.read.table(read.char,skipline=1+line.tmp,startpoint=target.line-10,gyou=1,
                                      table.property=cl,
                                      outfile=repfile,h=FALSE,is.ss2=TRUE,colClasses="character")  
    caa <- find.and.read.table(read.char,skipline=abs(line.tmp),startpoint=name.label[[2]]-10,gyou=NULL,colClasses="character",
                               table.property=cl,comment.char="",fill=T,
                               outfile=repfile,h=FALSE,is.ss2=TRUE)
  }
  colnames(caa[[1]]) <- as.character(name.label[[1]])  

  if(!is.ss3(repfile)){  # The case older than SS3
    elimit.line <- fleet <- area <- gmorph <- numeric()
    s <- 0
    for(i in 1:nrow(caa[[1]])){
      if(caa[[1]][i,1]=="fleet"){
        s <- s+1      
        elimit.line[s] <- i
      }
      fleet[i] <- caa[[1]][elimit.line[s],2]
      area[i] <- caa[[1]][elimit.line[s],4]
      gmorph[i] <- caa[[1]][elimit.line[s],6]        
    }
    YQ <- as.numeric(caa[[1]]$Year)+(as.numeric(caa[[1]]$Seas)/4)-0.25
    caa[[1]] <- cbind(fleet,area,gmorph,YQ,caa[[1]])
    caa[[1]] <- caa[[1]][c(-elimit.line,-(elimit.line+1)),]
  }
  else{
    caa[[1]]$Year <- caa[[1]]$Yr
    caa[[1]]$fleet <- caa[[1]]$Fleet
    caa[[1]]$YQ <- as.numeric(caa[[1]]$Year)+(as.numeric(caa[[1]]$Seas)/4)-0.25
  }

  age.cols <- !is.na(as.numeric(dimnames(caa[[1]])[[2]]))
  tmp <- !is.na(caa[[1]]$YQ)
  caa.array <- array(0,dim=c(length(unique(caa[[1]]$YQ[tmp])),
                         sum(age.cols),length(unique(caa[[1]]$fleet))))
  
  s <- 1
  #  browser()
  #  startcol <- ifelse(is.ss3(repfile),11,7)
  for(i in which(age.cols)){
    caa.array[,s,] <- tmp2 <- tapply(as.numeric(caa[[1]][tmp,i]),
                                    list(caa[[1]]$YQ[tmp],
                                         as.factor(unfactor(caa[[1]]$fleet[tmp]))),sum)
    s <- s+1
  }
  dimnames(caa.array) <- list(rownames(tmp2),colnames(caa[[1]])[age.cols],colnames(tmp2))    
  list(caa=caa[[1]],target.line=caa[[2]],caa.array=caa.array)
}
