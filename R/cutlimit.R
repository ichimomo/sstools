cutlimit <-
function (smp, lim = 1000) 
{
    smp[smp > lim] <- lim
    smp
}
