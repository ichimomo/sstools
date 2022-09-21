find.and.read.table3 <-
function (findseq, skipline, gyou, table.property, tb, outfile, 
    h = TRUE, is.ss2 = FALSE, target.line = NULL, ...) 
{
    if (is.null(target.line)) {
        tmp <- 1:nrow(tb)
        target.line <- tmp[tb[, 1] == findseq]
    }
    res <- as.list(rep(0, length(target.line) + 1))
    gyou0 <- rep(0, length(target.line))
    for (k in 1:length(target.line)) {
        if (is.null(gyou)) {
            gyou0[k] <- count.next.line(target.line[k], skipline + 
                1, table.property[1:nrow(tb)]) - target.line[k] - 
                skipline
        }
        else {
            gyou0[k] <- gyou
        }
        a <- read.table(outfile, skip = target.line[k] + skipline, 
            header = FALSE, nrow = gyou0[k], fill = T, col.names = c("V", 
                1:max(cl)), ...)
        a <- a[, apply(!is.na(a), 2, sum) != 0]
        if (h == TRUE) {
            a.name <- read.table(outfile, skip = target.line[k] + 
                skipline - 1, header = FALSE, nrow = 1, colClasses = "character")
            dimnames(a) <- list(a[, 1], as.character(a.name))
        }
        if (is.ss2 == FALSE) {
            a <- a[, -1]
        }
        res[[k]] <- a
    }
    res[[k + 1]] <- target.line
    res
}
