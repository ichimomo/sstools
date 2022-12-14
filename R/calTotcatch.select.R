calTotcatch.select <-
function(repfile,target.fleet=0,target.term=0,
                               future.res=NULL,
                               error.func="select",
                               len.sd=0,
                               # error.funcは、"multinom"、"select" または "both"
                               # len.sdは"select"または"both"のときのみ使う
                               # length.sample.size の1つ目は真の、2つ目はdatファイルのインプットのもの
                               length.sample.size=c(100,9)){
  
  # error function can be selected from "select" or "multinom"
#  biom <- getBabs.ss2(repfile)

  cl0 <- count.fields(repfile,blank.lines.skip=FALSE)
  tb0 <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl0),sep=""),as.is=T,
                    blank.lines.skip=FALSE,colClasses=rep("character",max(cl0)),
#                    nrows=length(cl0)/2)[,1:2]
                    )[,1:2]                      

  naa <- getNAA.ss2(repfile,cl=cl0,tb=tb0)[[1]]
  caa <- getCAA.ss2(repfile,cl=cl0,tb=tb0)
  select <- t(getSelect.ss2(repfile,cl=cl0,tb=tb0)[[1]])
  aselect <- t(getSelect.ss2(repfile,cl=cl0,tb=tb0,len=F)[[1]])
  alk <- getALK.ss(repfile,all=TRUE,cl=cl0,tb=tb0)[[1]]
  WatL <- getWatL.ss2(repfile,cl=cl0,tb=tb0)[[1]]
  nma <- getNMA.ss2(repfile,cl=cl0,tb=tb0)[[1]]
  faa <- calFAA.ss2(repfile,datas=list(naa=naa,caa=caa,nma=nma))  
  
  tmp <- unlist(strsplit(rownames(aselect),"-"))
  label.aselect <- as.data.frame(t(matrix(tmp,3,length(tmp)/3)))
  tmp <- unlist(strsplit(rownames(select),"-"))
  label.select <- as.data.frame(t(matrix(tmp,3,length(tmp)/3)))

  colnames(label.select) <- colnames(label.aselect) <- c("fleet","year","Gmorph")
  label.select$fleet <- substr(as.character(label.select$fleet),2,3)
  label.select$year <- substr(as.character(label.select$year),2,5)
  label.aselect$fleet <- substr(as.character(label.aselect$fleet),2,3)
  label.aselect$year <- substr(as.character(label.aselect$year),2,5)    

  if(is.null(future.res)){
    years <- as.numeric(dimnames(caa$caa.array)[[1]])
  }
  else{
    years <- as.numeric(rownames(future.res$naa))
  }
  nfleet <- dim(caa$caa.array)[[3]]
  tmp.array <- caa$caa.array
  tmp.array[] <- 0
  wcaa.array <- caa$caa.array
  wcaa.array[] <- 0

  target.res <- list()  
  s <- 1

  nbins <- nrow(alk[[1]])-2

  for(i in 1:length(years)){
    qt <- qtback(years[i])

    if(1){ # <- Qを計算する場合は、全漁業込みのN_childarを使っているみたい。
      if(is.null(future.res)){        
        tmp <- naa$Year==floor(years[i]) & naa$Seas==qt
        tmp.faa <- as.numeric(faa$faa[rownames(faa$faa)==years[i],])
        # numbers at age by bin and age
        tmpline <- ifelse(is.ss3(repfile),11,4)
        naa.beg <- as.numeric(naa[tmp,tmpline:ncol(naa)])
        naa.mid <- naa.beg * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))      
      }
      else{
        tmp <-  years==years[i]
        tmp.qt <- qtback(colnames(future.res$faa))==qt
        tmp.faa <- future.res$faa[tmp,tmp.qt]
        naa.mid <- future.res$naa[tmp,tmp.qt] * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))      
      }

      if(0){ # ALKはmiddle of season かと思ったけど、違う
        alk[[max(nma$Seas)+1]] <- alk[[1]]
        alk.tmp <- (as.matrix(alk[[qt]][1:nbins,-1]) + as.matrix(alk[[qt+1]][1:nbins,-1]))/2
        n.ba <- sweep(alk.tmp,2,naa.mid,FUN="*")
      }
      if(1){
        n.ba <- sweep(as.matrix(alk[[qt]][1:nbins,-1]),2,naa.mid,FUN="*")
      }
        rownames(n.ba) <- alk[[1]][1:nbins,1]
      n.ba <- n.ba[nrow(n.ba):1,]
    }

    # by fleet
    for(j in 1:nfleet){

      if(0){ #----technicalマニュアルには、N_childerは漁業によって異なるとなっている
             # が、それは間違い
        if(is.null(future.res)){        
          tmp <- naa$Year==floor(years[i]) & naa$Seas==qt
          tmp.faa <- as.numeric(faa$faa.array[dimnames(faa$faa)[[1]]==years[i],,j])
          tmpline <- ifelse(is.ss3(repfile),11,4)
          naa.beg <- as.numeric(naa[tmp,tmpline:ncol(naa)])
          naa.mid <- naa.beg * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))
        }
        else{
          tmp <-  years==years[i]
          tmp.qt <- qtback(colnames(future.res$faa))==qt
          tmp.faa <- future.res$faa[tmp,tmp.qt]
          naa.mid <- future.res$naa[tmp,tmp.qt] * exp(-0.5 * (nma$M[nma$Seas==qt]/max(nma$Seas)+tmp.faa))
        }
        n.ba <- sweep(as.matrix(alk[[qt]][1:nbins,-1]),2,naa.mid,FUN="*")        
        rownames(n.ba) <- alk[[1]][1:nbins,1]
        n.ba <- n.ba[nrow(n.ba):1,]
      }
      
      tmp <- label.aselect$fleet==j & label.aselect$year==max(naa$Year[naa$Per=="TIME"])#& label.aselect$year<=years[i] # !!!!!! made temporary change
      beta.a <- aselect[max(which(tmp)),]  # age selectivity

      tmp <- label.select$fleet==j & label.select$year==max(naa$Year[naa$Per=="TIME"])# & label.select$year<=years[i] !!!!!!!! temporary change 
      beta.l <- select[max(which(tmp)),]  # length selectivity
      n.b <- sweep(n.ba,2,beta.a,FUN="*") # age x length selctivity

      # vulnerable numbers (n.l) and weights (w.l) at age  by length
      n.l <- sweep(n.b,1,beta.l,FUN="*") 
      w.l <- sweep(n.l,1,WatL$Wt,FUN="*") 

      # vulnerable numbers at age (summed vuluneragble numbers at age by age)
      x <- apply(n.l,2,sum)

      #-------- Record vulnerable biomass
      if(j==1 && i==1){
        vna <- vba <- array(0,dim=c(length(x),nfleet,length(years)))
        dimnames(vna) <- dimnames(vba) <-list(1:length(x),1:nfleet,years)
      }
      vna[,j,i] <- x
      vba[,j,i] <- apply(w.l,2,sum)
      #-------- end

      # -------- Record expected length at age
      # expected length freq ==> 実際のcatch at ageに合わせたc.alとn.lがあるが、ここではn.lを使う
#      browser()
      if(error.func=="select"){
        obs.len.tmp <- apply(sweep(n.l,2,exp(rnorm(dim(n.l)[[2]],mean=0,sd=len.sd)),FUN="*"),1,sum)
      }
      else{
        if(error.func=="multinom"){
#          obs.len.tmp <- apply(sweep(n.l,2,exp(rnorm(dim(n.l)[[2]],mean=0,sd=len.sd)),FUN="*"),1,sum)
          obs.len.tmp <- apply(n.l,1,sum)
          obs.len.tmp <- rmultinom(1,length.sample.size[1],obs.len.tmp)
#          browser()
        }
        else{
          if(error.func=="both"){
            obs.len.tmp <- apply(sweep(n.l,2,exp(rnorm(dim(n.l)[[2]],mean=0,sd=len.sd)),FUN="*"),1,sum)
            obs.len.tmp <- rmultinom(1,length.sample.size[1],obs.len.tmp)
#            browser()
          }
          else{
            obs.len.tmp <- apply(n.l,1,sum)
          }
        }
      }
      
#      names(obs.len.tmp) <- names(n.b)
#      browser()
      obs.len.tmp <- c(floor(years[i]),qtback(years[i]),j,1,0,
                       length.sample.size[2],obs.len.tmp/sum(obs.len.tmp))
      
      if(j==1 && i==1){
        obs.len <- obs.len.tmp
      }
      else{
        obs.len <- rbind(obs.len,obs.len.tmp)
      }
      
      # actual catch at age (catch at age from repfile)
      if(is.null(future.res)){              
        x0 <- caa$caa.array[i,,j] # i <- year, j <- fleet
#--------- future projectionの場合は、必要ない      
#      else{
#        x0 <- future.res$pcaa[i,j,]
#      }

        tmp <- x0/x   
        tmp[tmp==Inf|is.nan(tmp)] <- 0
        # mutiply vulunerable number by exploitation rate -> catch at age and length
        # tmpは、c.alから計算されるcatch at ageがReport.ssoのcatch at ageと一致するように調整している。なので、全体の漁獲量がぴったり合う。
        # 本来なら、x0とxのcompositionは全く一致する必要がある。この処理は漁獲量を合わせるための小手先。      
        c.al <- sweep(n.l,2,tmp,FUN="*")

        #--------- 2kg以下を規制する等のシナリオのときに使う
        if(target.fleet==j & years[i] >= target.term[1]  & years[i] <= target.term[2]){
          target.res[[s]] <- c.al
          target.res[[s]] <- ifelse(is.nan(target.res[[s]]),0,target.res[[s]])
          names(target.res)[s] <- paste2("F",j,"Y",years[i])
          s <- s+1
        }

        wc.al <- sweep(c.al,1,WatL$Wt,FUN="*")
        wcaa.array[i,,j] <- apply(wc.al,2,sum) # catch weight at tf
        wcaa.array[i,is.nan(wcaa.array[i,,j]),j]  <- 0
        tmp.array[i,,j] <- tmp
      }
    }
  }

  dimnames(obs.len) <- NULL

  list(exp.rage=tmp.array,
       wcaa.array=wcaa.array,
       target.res=target.res,
       datas=list(naa=naa,caa=list(caa=caa$caa.array,caa.array=caa$caa.array),
         nma=nma,vna=vna,vba=vba,obs.len=obs.len))
}
