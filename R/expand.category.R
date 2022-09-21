expand.category <-
function (num, category, label) 
{
    n <- sum(num)
    res <- data.frame(label = factor(NA, level = levels(label)), 
        category = rep(0, n))
    s <- 0
    for (i in 1:length(num)) {
        range <- (s + 1):(s + num[i])
        res$label[range] <- label[i]
        res$category[range] <- category[i]
        s <- s + num[i]
    }
    res
}
