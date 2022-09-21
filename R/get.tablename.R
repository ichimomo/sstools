get.tablename <-
function (x, FUN = "names") 
{
    tmpfun <- get(FUN)
    xlabel <- tmpfun(x)
    return(as.numeric(as.character(xlabel)))
}
