bindmat2 <-
function (mat, n) 
{
    mat2 <- matrix(0, nrow(mat) * n, ncol(mat))
    s <- 1
    for (i in 1:nrow(mat)) {
        for (j in 1:n) {
            mat2[s, ] <- mat[i, ]
            s <- s + 1
        }
    }
    mat2
}
