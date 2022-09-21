read.fleetname <-
function(datfile){
    cf <- count.fields(datfile,comment="",blank.lines.skip=T)
    a <- read.table(datfile,fill=T,col.names=paste("V",1:max(cf),sep=""),as.is=T)
#    fleet.name <- strsplit(a[8,1],"%")[[1]]
    fleet.name <- strsplit(a[9,1],"%")[[1]]    
    fleet.name
}
