paste.new <-
function (vec) 
{
    n <- length(vec)
    moji <- NULL
    for (i in c(1:n)) moji <- paste(moji, vec[i])
    moji
}
