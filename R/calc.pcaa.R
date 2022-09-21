calc.pcaa <-
function (caa1, naa1, maa1) 
{
    exp.rate <- pcaa <- caa1
    for (k in 1:dim(caa1)[3]) {
        for (i in 1:dim(caa1)[1]) for (j in 1:dim(caa1)[2]) {
            if (caa1[i, j, k] != 0) {
                exp.rate[i, j, k] <- caa1[i, j, k]/naa1[i, j]
                pcaa[i, j, k] <- calc.pcaa.tmp(exp.rate[i, j, 
                  k], maa1[j])
            }
            else {
                pcaa[i, j, k] <- 0
            }
        }
        cat(k, " ")
    }
    pcaa
}
