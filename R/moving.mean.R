moving.mean <-
function (y, y.name = NULL, kukan = 3) 
{
    m <- (kukan - 1)/2
    res <- res.name <- rep(0, length(y) - kukan + 1)
    s <- 1
    for (i in (m + 1):(length(y) - m)) {
        res[s] <- mean(y[(i - m):(i + m)])
        if (!is.null(y.name)) {
            res.name[s] <- y.name[i]
        }
        s <- s + 1
    }
    if (is.null(y.name)) 
        res.name <- 1:length(res)
    list(x = as.numeric(res.name), y = res)
}
