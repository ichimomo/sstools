rowtapply <-
function (a0) 
{
    yname <- floor(as.numeric(rownames(a0)))
    res <- matrix(0, length(unique(yname)), ncol(a0))
    for (i in 1:ncol(a0)) {
        res[, i] <- tapply(a0[, i], yname, sum)
    }
    dimnames(res) <- list(unique(yname), colnames(a0))
    res
}
