read.bootfiles <-
function(bootfiles,calc.PS=FALSE){
  nboot <- length(bootfiles)
  biom <- list()
  for(i in 1:nboot){
      biom[[i]] <- getBabs.ss2(bootfiles[i])[[1]]
      cat(i," ")
    }

  biom.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    biom.mat[,i] <- biom[[i]]$SpawnBio
  }
  rownames(biom.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  biom.mat <- as.data.frame(t(biom.mat))
  
  rec.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    rec.mat[,i] <- biom[[i]]$"recruit-0"
  }
  rownames(rec.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  rec.mat <- as.data.frame(t(rec.mat))

  biomass.mat <- matrix(0,nrow(biom[[1]]),nboot)
  for(i in 1:nboot){
    biomass.mat[,i] <- biom[[i]]$"bio-all"
  }
  rownames(biomass.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
  biomass.mat <- as.data.frame(t(biomass.mat))  

  # total catch # previous version
#  tc.mat <- matrix(0,nrow(biom[[1]]),nboot)
#  for(i in 1:nboot){
#    tmp <- apply(a[,substr(colnames(biom[[i]]),1,9)=="ret_catch"],1,sum)  # total catch
#    tc.mat[,i] <- tapply(tmp,biom[[i]]$year,sum)
#  }
# rownames(tc.mat) <- biom[[1]]$year+biom[[1]]$season/4-0.25
#  tc.mat <- as.data.frame(t(tc.mat))

  # Calculate population size
  if(calc.PS==FALSE){
    population.size <- NULL
  }
  else{
#    population.size <- rec.mat
#    population.size[] <- 0
    for(i in 1:nboot){
      a <- getNAA.ss2(bootfiles[i])[[1]]
      b <- getNMA.ss2(bootfiles[i])[[1]]
      x <- apply(sweep(a[,-1:-10],2,b$Age_Mat[b$Seas==4],FUN="*"),1,sum)
      if(i==1){
        population.size <- matrix(0,nboot,length(x))
        dimnames(population.size) <- list(1:nboot,a$Year+a$Seas/4-0.25)
      }
      population.size[i,] <- x
    }
  }
  
  return(list(biom.list=biom,rec.mat=rec.mat,biom.mat=biom.mat,population.size=population.size,biomass.mat=biomass.mat))
  
}
