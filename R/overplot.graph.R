overplot.graph <-
function (mat, col.var = NULL, ...) 
{
    y <- apply(mat, 1, sum)
    x <- as.numeric(names(y))
    if (length(x) == 0) 
        x <- 1:length(y)
    plot(x, y, type = "n", ylim = c(0, max(y, na.rm = T)), ...)
    if (is.null(col.var)) 
        col.var <- gray(seq(from = 0.1, to = 0.9, length = ncol(mat)))
    mat <- cbind(0, mat)
    s <- rep(0, nrow(mat))
    for (i in 2:ncol(mat)) {
        polygon(c(x, rev(x)), c(rev(s), s <- s + rev(mat[, i])), 
            col = col.var[i])
    }
}
