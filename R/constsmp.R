constsmp <-
function (smp, dev = 100, lim = NULL) 
{
    if (!is.null(lim)) {
        rep(lim, length(smp))
    }
    else {
        rep(round(mean(smp)/dev), length(smp))
    }
}
