is.ss3 <-
function (repfile)
{

    tmp <- sapply(repfile,FUN=function(x){
            a<-read.csv(x, nrow = 1, colClasses = "character",
                  header = F)
            tmp0 <- as.numeric(substr(a[1, 1], 5, 8)) > 3 | as.numeric(substr(a[1,
        1], 3, 6)) > 3
        if (is.na(tmp0)) tmp0 <- FALSE
        tmp0
      })
    tmp
}
