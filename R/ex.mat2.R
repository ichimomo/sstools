ex.mat2 <-
function (mattemp, rm.zero = FALSE) 
{
    a1 <- rep(rownames(mattemp), ncol(mattemp))
    a2 <- rep(0, n <- ncol(mattemp) * nrow(mattemp))
    for (i in c(1:n)) a2[i] <- colnames(mattemp)[ceiling(i/nrow(mattemp))]
    a3 <- c(mattemp)
    res <- data.frame(x = a1, y = a2, num = as.numeric(a3))
    if (rm.zero == TRUE) 
        res <- res[res$num != 0, ]
    res
}
