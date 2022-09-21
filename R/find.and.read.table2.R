find.and.read.table2 <-
function (findseq, skipline, gyou, comment.char = "#", table.property, 
    tb, outfile, h = TRUE, is.ss2 = FALSE, gyou.margin = 0, target.line = NULL, 
    ...) 
{
    if (is.null(target.line)) {
        tmp <- 1:nrow(tb)
        target.line <- charmatch(findseq, tb[, 1])
    }
    if (length(target.line) == 0) 
        cat("Cannot find target line!!!")
    if (is.null(gyou)) 
        gyou <- count.next.line(target.line, skipline, table.property[1:nrow(tb)]) - 
            target.line - skipline - gyou.margin
    a <- read.table(outfile, skip = target.line + skipline, header = FALSE, 
        nrow = gyou, comment.char = comment.char, ...)
    if (h == TRUE) {
        a.name <- read.table(outfile, skip = target.line + skipline - 
            1, header = FALSE, nrow = 1, colClasses = "character")
        dimnames(a) <- list(a[, 1], as.character(a.name))
    }
    if (is.ss2 == FALSE) {
        a <- a[, -1]
    }
    list(a, target.line + gyou)
}
