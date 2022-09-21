month2days <-
function (month, day) 
{
    x <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    x1 <- numeric()
    if (length(month) == 1) 
        x1 <- sum(x[0:(month - 1)]) + day
    else {
        for (i in c(1:length(month))) x1[i] <- sum(x[0:(month[i] - 
            1)]) + day[i]
    }
    x1
}
