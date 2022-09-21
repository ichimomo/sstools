plotlength.fit <-
function(repfile,comps=NULL,lty.var=rep(1,length(repfile)),col.var=1:length(repfile),
                           lwd.var=rep(1,length(repfile)),compfile=NULL,tb=NULL,cl=NULL,
                           kind="LEN",len.residual.plot=FALSE,nma.list=NULL){
#-------- This function is only for SS3 (no SS2)
  len.rep <- nrep <- length(repfile)
  multiplot <- nrep > 1

  if(is.null(comps)){
    comps <- as.list(1:nrep)
    for(i in 1:len.rep){
      tmp <- readcompfile(repfile[i],cl=cl[[i]],tb=tb[[i]],len=len,compfile=compfile[i])
      comps[[i]] <- tmp[[1]]
  }}

  len.data <- comps[[1]][comps[[1]]$kind==kind,]
#  fleet.row <- sort(unique(comps[[1]]$fleet))
  fleet.row <- sort(unique(len.data$fleet))  
  fleet.name <- 1:length(fleet.row)
#  browser()
 
  # Pearson residual of length data:
  if(len.residual.plot==TRUE){
    par(mfrow=c(3,1),mar=c(3,3,1,1))
    s <- 1
    for(i in fleet.row){
      median.list <- list()
      tmp <- len.data[len.data$fleet==i,]
      y <- tmp$Pearson
      x <- tmp$bin
      boxplot(y~x,ylim=c(-3,6),xlab=kind,ylab="Pearson residuals",
              boxwex=ifelse(multiplot,0.2,0.8),pch=".",lwd=0.5,col="gray")
      s <- s+1
      #        boxplot(y <- tmp$Pearson~tmp$bin,at=sort(unique(tmp$bin)),ylim=c(-3,6),xlab="Length",ylab="Pearson residuals")
      abline(h=0,col="red")
      #        title(main=paste("Fleet",i,":",ifelse(!is.na(fleet.name[i]),fleet.name[i],"")),line=-1)
      title(main=paste("Fleet",i),line=-1)
      #    sd.tmp <- (tmp$obs-tmp$exp)/sqrt(tmp$exp*(1-tmp$exp)*tmp$N)
      #    plot(tmp$bin,sd.tmp,
#        if(!multiplot){
      median.list[[1]] <- tapply(y,x,median,na.rm=T)
#          x2 <- tapply(y,tmp$bin,mean)  
#          points(x1,type="l",col=1)
#          points(x2,type="l",col="blue")
#        }
#        else{
      if(multiplot){
        for(j in 2:len.rep){
          tmp <- comps[[j]][comps[[j]]$fleet==i,]
          y <- tmp$Pearson            
          x <- tmp$bin
          boxplot(y~x,ylim=c(-3,6),xlab="",ylab="",boxwex=0.2,add=T,names=NA,axes=F,
                  at=(1:length(unique(x[!is.na(x)]))) + (j/(len.rep+2)),col=col.var[j],pch=".",lwd=0.5)
#            points(tmp$bin,y <- tmp$Pearson,col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
#            points(tmp$bin,y <- tmp$Pearson)
#            points(tapply(y,x,median),type="l",col=col.var[j],lty=lty.var[j],lwd=lwd.var[j])
          median.list[[j]] <- tapply(y,x,median,na.rm=T)
        }}
      for(j in 1:len.rep){
        points((1:length(median.list[[j]]))+0.5,# (j/(len.rep+2)),
               median.list[[j]],col=col.var[j],lty=lty.var[j],lwd=lwd.var[j],type="l")
#          browser()
#          if(i==rev(fleet.row)[1]) browser()
      }
      abline(h=0,col="gray")
      if(s%%3==0) mtext(side=3,line=0.5,adj=0.1,
              paste2("@ ",kind," residuals (by each bin)"),outer=T)      
    }
    if(s<3)  mtext(side=3,line=0.5,adj=0.1,
              paste2("@ ",kind," residuals (by each bin)"),outer=T)      
  }
#    }

  ## Size frequency 2
  length.bin <- sort(unique(len.data$bin))
  sum.length <- list(array(0,dim=c(length(length.bin),length(fleet.row),2)))
  dimnames(sum.length[[1]]) <- list(length.bin,fleet.row,c("Obs","Exp"))
  if(multiplot){
    for(kk in 2:len.rep){
      sum.length[[kk]] <- sum.length[[1]]
    }
  }

  #-- データタイプの種類
  x <- lapply(apply(comps[[1]][,1:9],2,unique),function(x) x[!is.na(x) & x!=""])
  pickcolumn <- which(sapply(x,length)>1)
  
  row.name <- #paste("F:",len.data$fleet,"-Y:",floor(as.numeric(len.data$year)),
              #      "-S:",len.data$season,sep="")
    apply(len.data[,pickcolumn],1,paste,collapse="-")
#もしかしてもう tmp.refは必要ない？ 昔は、comps[[1]][[2]]を見ないとNがわかんなかったが、、。
  original.uniquerow <- unique(row.name)
#  tmp.ref <- paste("F:",comps[[1]][[2]]$Index,
#                   "-Y:",floor(as.numeric(comps[[1]][[2]]$Year)),
#                  "-S:",comps[[1]][[2]]$Seas,sep="")
  s <- 1
  if(multiplot){
    row.name.list <- tmp.ref.list <- b.tmp <- list()
    for(k in 2:len.rep){
      b.tmp[[k]] <- comps[[k]][comps[[k]]$kind==kind,]
      row.name.list[[k]] <-  apply(b.tmp[[k]][,pickcolumn],1,paste,collapse="-")
    }}
  
#  par(bg=gray(0.9))
  par(mar=c(1.5,2,1,1),mfcol=c(12,2),oma=c(2,2,2,2))    
  for(j in 1:length(fleet.row)){
    for(i in 1:length(original.uniquerow)){
      row.tmp <- row.name==original.uniquerow[i]
      if(sum(len.data$fleet[row.tmp]==fleet.row[j],na.rm=T)){
        x <- len.data$bin[row.tmp]
        y <- cbind(len.data$obs[row.tmp],
                   len.data$exp[row.tmp])*ifelse(all(is.na(len.data$N[row.tmp])),1,len.data$N[row.tmp])
                     #comps[[1]][[2]]$Nsamp[tmp.ref==original.uniquerow[i]]
        x <- x[!is.na(x)]
        y <- subset(y,!is.na(y[,1]))
        if(s%%12==0){
          par(mar=c(1.5,2,0,1))
          plot(range(len.data$bin,na.rm=T),c(0,max(y,na.rm=T)),type="n",xlab="",ylab="")          
        }
        else{
          par(mar=c(0,2,0,1))
          plot(range(len.data$bin,na.rm=T),c(0,max(y,na.rm=T)),type="n",xaxt="n",xlab="",ylab="")          
        }
        if(!is.null(nma.list)){
          current.season <- as.numeric(substr(strsplit(original.uniquerow[i],"-")[[1]][3],3,4))
          for(ss in len.rep:1){
            abline(v=subset(nma.list[[ss]],Seas==current.season)$"Len_Mid",lty=2,
                   col=ifelse(ss>1,col.var[ss],"white"))
          }
        }
        polygon(c(min(x)-1,x,max(x)+1),c(0,y[,1],0),col="skyblue",border="skyblue")
#        matpoints(x,y,col=c(1,1),type=c("b","l"),                
#                pch=1:2,lty=1:1,cex=0.7,ylab="",xlab="")        
        points(x,y[,2],col=c(1,1),type=c("l"),                
                pch=1:2,lty=1:1,cex=0.7)
        ## Sum up size data by fisheries
        y <- y[!is.na(y[,1]),]
        x <- x[!is.na(x)]
        sum.length[[1]][match(x,length.bin),j,] <- sum.length[[1]][match(x,length.bin),j,]+y
        ##
        title(main=paste(original.uniquerow[i]),line=-0.85,adj=0.1,cex.main=1,font.main=1)
#        legend("topleft",legend=paste(original.uniquerow[i]),bg="white",box.col ="white")
 #       browser()

        ## For multiple plots ##
        if(multiplot){
            for(k in 2:len.rep){
              row.tmp <- row.name.list[[k]]==original.uniquerow[i]
              if(sum(row.tmp)==0){
                cat("warning!!",original.uniquerow[i], "can't be found in ",repfile[k],". Then skipped.  \n")
#                browser()
              }
              else{
                x <- b.tmp[[k]]$bin[row.tmp]
                y <- cbind(b.tmp[[k]]$obs[row.tmp],
                           b.tmp[[k]]$exp[row.tmp]) * ifelse(all(is.na(b.tmp[[k]]$N[row.tmp])),1,b.tmp[[k]]$N[row.tmp])
                             #b.tmp[[2]]$Nsamp[tmp.ref.list[[k]]==original.uniquerow[i]]
                points(x,y[,2],col=col.var[k],lty=lty.var[k],lwd=lwd.var[k],type="l")
                y <- y[!is.na(y[,1]),]
                x <- x[!is.na(x)]
                sum.length[[k]][match(x,length.bin),j,] <- sum.length[[k]][match(x,length.bin),j,]+y
#                if(!is.null(nma.list)){
#                  abline(v=subset(nma.list[[k]],Seas==current.season)$"Len_Mid",lty=2,col=col.var[k],lwd=0.5)
#                }                
              }
            }}

        if(s%%24==0){
#          browser()          
          mtext(side=3,line=0.5,adj=0.1,
                paste2("@ ",kind," fit (by each strata, line: expected, polygon: observed) \'",
                       paste(colnames(len.data)[pickcolumn],collapse="-"),"\'"),outer=T)
        }
        s <- s+1                        
      }
    }}
  if(s<24){
    mtext(side=3,line=0.5,adj=0.1,
          paste2("@ ",kind," fit (by each strata, line: expected, polygon: observed)",
                 paste(colnames(len.data)[pickcolumn],collapse="-")),outer=T)
  }  
  cat(paste2("Plot of expected and observed ",kind," data by each observation was done.  \n"))
    
  n <- dim(sum.length[[1]])[[2]]
  setncol(n)
  par(mar=c(3,3,1,1),bg="white")  
    for(i in 1:n){
      x <- length.bin
      y <- sum.length[[1]][,i,]
#      browser()
      matplot(x,y,type=c("n","n"),lty=1,
              pch=1,ylab="nsmple",col=c("black",1),xlab=kind)
      y.tmp <- y[y[,2]>0,]
      x.tmp <- x[y[,2]>0]      
      polygon(c(min(x.tmp)-1,x.tmp,max(x.tmp)+1),c(0,y.tmp[,1],0),col=c("skyblue"),border="skyblue")
      points(x.tmp,y.tmp[,2],type=c("l"),lty=1,pch=1,col=c("black"))                    
#      matplot(x,y,type=c("b","l"),lty=1,
#              pch=1,ylab="nsmple",col=c("black",1),xlab="Length (cm)")              
      title(paste("Fleet",fleet.row[i]))

      if(multiplot){
          for(k in 2:len.rep){
            points(length.bin[sum.length[[k]][,i,2]>0],
                   sum.length[[k]][sum.length[[k]][,i,2]>0,i,2],
                   type="l",col=col.var[k],lty=lty.var[k],lwd=lwd.var[k])
          }}
      
      if(n>10 && i%%32==0){
        mtext(side=3,line=0.5,adj=0.1,
              paste2("@ ",kind," fit (by fleet, lines: expected, polygon: observed)"),outer=T)
      }}
    mtext(side=3,line=0.5,adj=0.1,paste2("@ ",kind," fit (by fleet, lines: expected, polygon: observed))"),
          outer=T)
    cat(paste2("Plot of expected and observed ",kind," frequency by fleets was done.  \n"))    

    # Bubble plot not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=T)    
    par(mfrow=c(3,1),mar=c(1.5,3,1.5,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"darkblue")
    ss  <- 1
    for(j in 1:length(fleet.row)){
      xxx <- len.data$fleet==fleet.row[j] & len.data$Pearson!=0 & !is.na(len.data$Pearson)
      if(sum(xxx)>0){
        year.range <- range(as.numeric(len.data[xxx,]$year))
        if(diff(year.range)<20){
          with(len.data[xxx,],symbols(as.numeric(year),bin,circles=sqrt(abs(x <- Pearson))/8,
                                      fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,ylab=kind[1],
                                      inches=FALSE,ylim=yrange,xlim=year.range))#as.numeric(xrange)))
          title(paste("Fleet",fleet.row[j],"(mean(abs(peason))="
                      ,round(mean(abs(len.data[xxx,]$Pearson)),3)))
#          browser()
          ss <- ss+1
        }
        else{
          ranges <- list(c(year.range[1],mean(year.range)+1),
                         c(mean(year.range)-1,year.range[2]))
#          browser()          
          for(kk in 1:2){
            with(len.data[xxx,],symbols(as.numeric(year),bin,circles=sqrt(abs(x <- Pearson))/8,
                                        fg=col.tmp[(x<0)+1],bg=col.tmp2[(x<0)+1],lwd=0.5,ylab=kind[1],
                                        inches=FALSE,ylim=yrange,
                                        xlim=ranges[[kk]]))
            title(paste2("Fleet ",fleet.row[j]," (",ranges[[kk]][1],"-",ranges[[kk]][2],")",
                         "(mean(abs(peason))=",round(mean(abs(len.data[xxx,]$Pearson)),3)))
            ss <- ss+1            
          }
#          browser()
        }
      }
      if(ss%%3==0)
        mtext(paste2("@ ",kind,": Bubble plot of residuals, darkblue (obs<exp), white (obs>exp) (FIRST repfile ONLY)"),
                     outer=T,adj=0.1)
    }

    # Bubble plot (observed length) not for multiplot
    xrange <- range(len.data$year)
    yrange <- range(len.data$bin,na.rm=T)    
    par(mfrow=c(3,1),mar=c(1.5,3,1.5,1))
    col.tmp <- c("black","black")
    col.tmp2 <- c(NA,"darkblue")
    ss  <- 1
    for(j in 1:length(fleet.row)){
      xxx <- len.data$fleet==fleet.row[j] & len.data$Pearson!=0 & !is.na(len.data$Pearson)
      if(sum(xxx)>0){
        year.range <- range(as.numeric(len.data[xxx,]$year))
        if(diff(year.range)<20){
#          browser()
          with(len.data[xxx,],symbols(as.numeric(year[x>0]),bin[x>0],circles=sqrt(abs(x <- obs[obs>0])),
                                      fg=1,bg=NA,lwd=0.5,ylab=kind[1],
                                      inches=0.1,
                                      ylim=yrange,xlim=year.range))#as.numeric(xrange)))
          title(paste("Fleet",fleet.row[j]))
#          browser()
          ss <- ss+1
        }
        else{
          ranges <- list(c(year.range[1],mean(year.range)+1),
                         c(mean(year.range)-1,year.range[2]))
#          browser()          
          for(kk in 1:2){
            with(len.data[xxx,],symbols(as.numeric(year[x>0]),bin[x>0],circles=sqrt(abs(x <- obs[obs>0])),
                                        fg=1,bg=NA,lwd=0.5,ylab=kind[1],
                                        ylim=yrange,inches=0.1,
                                        xlim=ranges[[kk]]))
            title(paste2("Fleet ",fleet.row[j]," (",ranges[[kk]][1],"-",ranges[[kk]][2],")"))
            ss <- ss+1            
          }
#          browser()
        }
      }
      if(ss%%3==0)
        mtext(paste2("@ ",kind,": Bubble plot of observed length comps (FIRST repfile ONLY)"),
                     outer=T,adj=0.1)
    }  
}
