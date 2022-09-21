getAgecomp.ss <-
function(repfile="ss2.rep",tb=NULL,
                                            cl=NULL,len=TRUE,compfile=NULL){#target.line=NULL,
  composition.database <- getAgecomp.ss2.2(repfile,tb=tb,cl=cl,compfile=compfile)
  
  fit.len.comps <- getAgecomp.ss2.1(repfile,tb=tb,cl=cl,len=len)
  list(composition.database[[1]],fit.len.comps[[1]],
       c(composition.database[[2]],fit.len.comps[[2]]))
}
