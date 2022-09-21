chline <-
function (char.tmp, n = 10) 
{
    res.char <- character()
    if (nchar(char.tmp) > n) {
        for (i in 1:ceiling(nchar(char.tmp)/n)) {
            if (i < ceiling(nchar(char.tmp)/n)) {
                res.char <- paste2(res.char, substr(char.tmp, 
                  1 + (i - 1) * n, n * i), "\n")
            }
            else {
                res.char <- paste2(res.char, substr(char.tmp, 
                  1 + (i - 1) * n, n * i))
            }
        }
    }
    else {
        res.char <- char.tmp
    }
    return(res.char)
}
