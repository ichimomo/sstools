good.colors <-
function () 
{
    tmp1 <- apply(col2rgb(colors()), 2, max) < 230
    tmp2 <- apply(col2rgb(colors()), 2, sd)/apply(col2rgb(colors()), 
        2, mean) > 0.5
    col.tmp <- colors()[tmp1 & tmp2 & !is.na(tmp1) & !is.na(tmp2)]
    a <- col2rgb(col.tmp)
    col.tmp <- col.tmp[order(a[1, ])]
    col.tmp <- matrix(col.tmp, 3, floor(length(col.tmp)/2), byrow = T)
    dim(col.tmp) <- NULL
    col.tmp
}
