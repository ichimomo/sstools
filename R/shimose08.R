shimose08 <-
function (t) 
{
    lmax <- 233.6
    lmin <- 21.54
    Amax <- 15
    Amin <- 0
    K <- 0.195
    linf <- lmin + (lmax - lmin)/(1 - exp(-K * (Amax - Amin)))
    linf + (lmin - linf) * exp((-K) * (t - Amin))
}
