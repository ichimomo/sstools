y.per.recruits <-
function (maa, waa, saa, fs) 
{
    wcs <- rep(0, length(fs))
    for (j in c(1:length(fs))) wcs[j] <- y.per.recruit(maa, waa, 
        saa, fs[j])
    wcs
}
