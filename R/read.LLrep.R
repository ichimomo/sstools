read.LLrep <-
function(repfile){
  cl <- count.fields(repfile,blank.lines.skip=FALSE)
  TLL <- find.and.read.table("LIKELIHOOD",skipline=0,startpoint=0,
                             table.property=cl,comment.char="",fill=T,gyou=NULL,
                             outfile=repfile,h=FALSE,is.ss2=TRUE)

  if(!is.ss3(repfile)){
    LLs <- find.and.read.table("Forecast_Recruitment",skipline=2,startpoint=0,
                               table.property=cl,comment.char="",fill=T,gyou=NULL,
                               outfile=repfile,h=TRUE,is.ss2=TRUE)
  }
  else{
    LLs <- find.and.read.table("Fleet:",skipline=0,startpoint=0,
                               table.property=cl,comment.char="",fill=T,gyou=NULL,
                               outfile=repfile,h=TRUE,is.ss2=TRUE)    
  }

  list(TLL=TLL[[1]],LLs=LLs[[1]])
}
