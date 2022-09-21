plot.data.frame <-
function(data,datalist=NULL,name,title.name,nline,col.var,lty.var,lwd.var,ptype){
  x <- data$Year
  y <- data[[name]]
  plot(x[y!=0],y[y!=0],ylab=title.name,ylim=c(0,max(y,na.rm=T)),
       xlab="Years",type=ptype,lwd=lwd.var[1])
  title(title.name,line=nline)
  if(!is.null(datalist)){
    for(i in 1:length(datalist)){
      x1 <- datalist[[i]]$Year
      y1 <- datalist[[i]][[name]]
      points(x1[y1!=0],y1[y1!=0],col=col.var[i+1],lty=lty.var[i+1],lwd=lwd.var[i+1],type="l")
      }}
}
