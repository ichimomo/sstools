getEffsmp1 <-
function (repfile = "ss2.rep") 
{
    pred.comp <- getAgecomp.ss2.1(repfile)
    pred.comp[[1]]$effN
}
