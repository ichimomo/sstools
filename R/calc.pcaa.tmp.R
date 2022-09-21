calc.pcaa.tmp <-
function (er1, maa1) 
{
    tmpfunc <- function(er1, maa1, pf1) {
        (er1 - pf1/(pf1 + maa1) * (1 - exp(-pf1 - maa1)))^2
    }
    optimize(tmpfunc, lower = 0, upper = 5, er1 = er1, maa = maa1)$minimum
}
