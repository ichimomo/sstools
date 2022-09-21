readSS22 <-
function(repfile="ss2.rep",...){
  res <- getBabs.ss2(repfile,...)[[1]]

  y.name <- res$year[-1:-2]+as.numeric(res$season[-1:-2])/4
  tmp <- res$"recruit-0"[-1:-2]
  
  list(biomass=list(x=y.name,y=res$"bio-all"[-1:-2]),
       SSB=list(x=y.name,y=res$"SpawnBio"[-1:-2]),
       recruit=list(x=y.name[tmp!=0],y=1000*tmp[tmp!=0]))       
}
