to.yrmonday <-
function (xyz, split = "/") 
{
    a <- strsplit(as.character(xyz), split = split)
    tmpfunc <- function(vec, n) {
        vec[n]
    }
    x <- lapply(a, tmpfunc, 1)
    y <- lapply(a, tmpfunc, 2)
    z <- lapply(a, tmpfunc, 3)
    data.frame(year = unfactor(x), month = unfactor(y), day = unfactor(z))
}
