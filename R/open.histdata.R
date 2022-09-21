open.histdata <-
function (num, bin) 
{
    res <- 1
    for (i in 1:length(num)) {
        if (num[i] != 0) {
            res <- c(res, rep(bin[i], num[i]))
        }
    }
    res[-1]
}
