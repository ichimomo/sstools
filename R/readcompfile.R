readcompfile <-
function(repfile="ss2.rep",cl=NULL,tb=NULL,
                                                compfile=NULL){ #target.line=NULL,
  #----- preliminary coding for adjusting 2.00o
  a <- read.csv(repfile,nrow=1,colClasses="character",header=F)
  if((is.ss2.2(repfile) && substr(a[1,1],16,20)>"2.00o") || (is.ss3(repfile) && vnumber.ss3(repfile)<3.03)){
    #--  for version from 2.02o to 3.02
    name.tmp <- c("year","season","fleet","rep","pick_gender",
                  "kind","mkt","ageerr","gender","Lbin_lo","Lbin_hi",
                  "bin","obs","exp","Pearson","N","effN","like","Used")
  }
  else{
    if(is.ss3(repfile) & vnumber.ss3(repfile)>=3.03){
      if(more.ss3.2(repfile)){
        name.tmp <- c("year","season","fleet","rep","pick_gender","kind","Part","ageerr","gender","Lbin_lo","Lbin_hi","bin","obs","exp","Pearson","N","effN","Like","Cum_obs","Cum_exp","SuprPer","Used?")        
      }
      else{
        #--  for newer than 3.03 (read compfile) 
        name.tmp <- c("year","season","fleet","rep","pick_gender","kind","Part","ageerr","gender","Lbin_lo","Lbin_hi","bin","obs","exp","Pearson","N","effN","Like","Cum_obs","Cum_exp","Used")
      }
      if(is.null(compfile)){
        stop(message="This version is newer than 3.30.  Please specify length composition file, named \"CompReport.SSO\" with argument of \"compfile=\"!")
      }
      else{
        repfile <- compfile
        cl <- count.fields(repfile,blank.lines.skip=FALSE)
        tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)        
      }
    }
    else{
      #--  for version older than 2.02o 
      name.tmp <- c("year","season","fleet","rep","pick_gender","kind",
                    "mkt","ageerr","gender","Lbin_lo","Lbin_hi","bin",
                    "obs","exp","Pearson","N","effN","Used")
    }}
  if(more.ss3.2(repfile)){
    type.tmp <- c("character",rep("numeric",4),"character",rep("numeric",length(name.tmp)-8),rep("character",2))    
  }
  else{
    type.tmp <- c("character",rep("numeric",4),"character",rep("numeric",length(name.tmp)-6))    
  }

  #----- preliminary coding for adjusting >3.00
  if(is.ss3(repfile)){
    gyou.margin <- 2
  }
  else{
    gyou.margin <- 0
  }

#  if(is.null(target.line)){
    if(is.null(cl)){
      cl <- count.fields(repfile,blank.lines.skip=FALSE)
    }
    if(is.null(tb)){
      tb <- read.table(repfile,fill=T,col.names=paste("V",1:max(cl),sep=""),as.is=T,
                       blank.lines.skip=FALSE)
    }
    res <- find.and.read.table2("Composition_Database",skipline=1,
                                gyou=NULL,table.property=cl,tb=tb,gyou.margin=gyou.margin,
                                outfile=repfile,h=FALSE,is.ss2=TRUE,fill=T,as.is=T,
                                colClasses=type.tmp,col.names=name.tmp)
  res
}
