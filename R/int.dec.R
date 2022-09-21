int.dec <-
function (cate, pre.cate = NA, interactive.mode = FALSE) 
{
    cate1 <- sort(unique(cate))
    n <- length(cate1)
    if (length(pre.cate) == 1) {
        cate.res <- rep(0, n)
    }
    else {
        cate.res <- pre.cate
    }
    if (interactive.mode == TRUE) {
        cat(as.character(cate1), "\n")
        for (i in 1:n) {
            cat(as.character(cate1[i]), ": ", as.character(pre.cate[i]), 
                " ")
            tmp <- readline()
            if (tmp != "n" & tmp != "AN") {
                cate.res[i] <- tmp
            }
            else {
                if (tmp == "AN") 
                  break
            }
        }
        cat("Do new category?")
        tmp <- readline()
    }
    else {
        tmp <- "Y"
        if (tmp == "Y") {
            new.dat <- rep(0, length(cate))
            for (i in 1:n) {
                new.dat[as.character(cate) == as.character(cate1)[i]] <- as.character(cate.res[i])
            }
        }
        else {
            new.dat <- NA
        }
    }
    return(list(category.list = cbind(as.character(cate1), cate.res), 
        newdat = new.dat))
}
