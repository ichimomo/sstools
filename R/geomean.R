geomean <-
function (x) 
{
    ifelse(all(x > 0), exp(mean(log(x))), NA)
}
