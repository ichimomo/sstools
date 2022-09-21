read.grad <-
function (parfile = "ss2.par") 
{
    a <- read.table(parfile, nrows = 1, comment = "")
    as.numeric(a[length(a)])
}
