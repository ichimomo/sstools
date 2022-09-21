getEffsmp2 <-
function (repfile = "ss2.rep") 
{
    pred.comp <- getAgecomp.ss2.1(repfile)
    b <- lm(pred.comp[[1]]$effN ~ pred.comp[[1]]$Nsamp)
    b$fitted.values
}
