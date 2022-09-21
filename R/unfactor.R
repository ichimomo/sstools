unfactor <-
function (x) 
{
    ch <- as.character(x)
    opt <- options(warn = -1)
    num <- as.numeric(ch)
    options(opt)
    if (any(is.na(num))) 
        ch
    else num
}
