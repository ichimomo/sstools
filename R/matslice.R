matslice <-
function (mat, num) 
{
    tmp <- rep(0, nrow(mat))
    x <- ceiling(ncol(mat)/num)
    s <- 0
    for (i in 1:x) {
        if (ncol(mat) < num) {
            tmp <- cbind(tmp, apply(as.matrix(mat[, 1:ncol(mat)]), 
                1, sum))
        }
        else {
            tmp <- cbind(tmp, apply(as.matrix(mat[, 1:num]), 
                1, sum))
            mat <- as.matrix(mat[, -1:-num])
            s <- s + 1
        }
    }
    tmp <- tmp[, -1]
    rownames(tmp) <- rownames(mat)
    colnames(tmp) <- num * 1:ncol(tmp)
    tmp
}
