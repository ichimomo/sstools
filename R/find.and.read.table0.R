find.and.read.table0 <-
function (findseq, skipline, startpoint, gyou, table.property, 
    outfile, h = TRUE, is.ss2 = FALSE, ...) 
{
    for (i in c(startpoint:(length(table.property) - 1))) {
        if (table.property[i] != 0 && read.table(outfile, skip = i - 
            1, nrow = 1)[1] == findseq) {
            if (is.null(gyou)) 
                gyou <- count.next.line(i, skipline, table.property) - 
                  i - skipline
            a <- read.table(outfile, skip = i + skipline, header = FALSE, 
                nrow = gyou, ...)
            if (h == TRUE) {
                a.name <- read.table(outfile, skip = i + skipline - 
                  1, header = FALSE, nrow = 1, colClasses = "character")
                dimnames(a) <- list(a[, 1], as.character(a.name))
            }
            if (is.ss2 == FALSE) {
                a <- a[, -1]
            }
            startpoint <- i + gyou
            break
        }
    }
    list(a, startpoint)
}
