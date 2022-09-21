bind.list2 <-
function (reslist) 
{
    reslist2 <- reslist[[1]]
    for (i in 1:(length(reslist) - 1)) {
        reslist2 <- rbind(reslist2, reslist[[i + 1]])
    }
    reslist2
}
