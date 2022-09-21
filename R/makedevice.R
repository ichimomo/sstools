makedevice <-
function(filename=NULL,dev.type="ps",filenum=NULL,htmlfile=NULL,
                       new=F,append=TRUE,tmp.par=par(),width=570,...){
  if(dev.type=="html"){
    if(new==T) dev.off()
    jpeg(file=n.tmp <- paste(filename,filenum,".jpeg",sep=""),
           quality=150,height=700,width=width,...)
#    par(tmp.par)
    cat(paste("<img src=\"./",n.tmp,"\" width=\"",width,"\" height=\"700\">\n",sep=""),file=htmlfile,append=append)
  }
  else{
 #  if(dev.type=="x11") locator(1)
  }
}
