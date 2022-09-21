plotgrowth2.ss <-
function(nmas,is.plot=TRUE,col.var=1:length(nmas),xlim=NULL,ylim=NULL,list.name=NULL,length.unit=""){
  if(is.null(list.name)) list.name <- 1:length(nmas)
    
  tmpfunc <- function(a){
    a <- a[order(a$"Age_Beg"),]
    label <- paste("M",a[,2],"B",a[,3],"Gen",a[,4],"submorph",a[,6],sep="-")
    ulabel <- unique(label)
    cbind(x <- tapply(a$"Len_Beg",list(a$"Age_Beg",label),sum),
          x+tapply(a$"SD_Beg",list(a$"SD_Beg",label),sum),
          x-tapply(a$"SD_Beg",list(a$"SD_Beg",label),sum))
  }

#  browser()
  b <- lapply(nmas,tmpfunc)
  legend.tmp <- pch.tmp <- col.tmp <- numeric()
  if(is.plot){
    if(is.null(xlim)) xlim <- max(as.numeric(unlist(lapply(b,function(x) rownames(x)))))
    if(is.null(ylim)) ylim <- max(as.numeric(unlist(lapply(b,function(x) x))))
    plot(c(0,xlim),c(0,ylim),type="n",xlab="Age",ylab=paste2("Length",length.unit))
    for(i in 1:length(b)){
      ulabel <- unique(colnames(b[[i]]))
      for(j in 1:length(ulabel)){
        matpoints(rownames(b[[i]]),b[[i]][,ulabel[j]==colnames(b[[i]])],type=c("o","l","l"),ltye=1,
                  col=col.var[i],pch=j)
        legend.tmp <- c(legend.tmp,paste(list.name[i],ulabel[j],sep=": "))
        col.tmp <- c(col.tmp,col.var[i])
        pch.tmp <- c(pch.tmp,j)
      }
    }
    legend("bottomright",legend=legend.tmp,col=col.tmp,pch=pch.tmp,lty=1)
  }

  invisible(b)
  
}
