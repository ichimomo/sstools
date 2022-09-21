tlist <-
function (list.x) 
{
    n <- length(list.x)
    list.x2 <- list.x
    for (i in c(1:n)) {
        list.x2[[n - i + 1]] <- list.x[[i]]
    }
    list.x2
}
