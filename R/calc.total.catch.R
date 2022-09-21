calc.total.catch <-
function (Fvec, Mvec, Nvec, Wvec) 
{
    n <- length(Mvec)
    tc <- 0
    for (i in c(1:n)) {
        tc <- tc + (Fvec[i]/(Fvec[i] + Mvec[i]) * (1 - exp(-Fvec[i] - 
            Mvec[i]))) * Nvec[i] * Wvec[i]/1000
    }
    tc
}
