bindmat <-
function (mat, n, row = T) 
{
    mat2 <- mat
    if (row == T) {
        for (i in 1:(n - 1)) mat2 <- rbind(mat2, mat)
    }
    else {
        for (i in 1:(n - 1)) mat2 <- cbind(mat2, mat)
    }
    mat2
}
