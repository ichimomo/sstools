getALK.ss2 <-
function (repfile = "ss2.rep", cl = NULL, tb = NULL, all = FALSE, 
    qt = 4) 
{
    vskipline <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile), 
        3, 6)
    if (is.null(cl)) {
        cl <- count.fields(repfile, blank.lines.skip = FALSE)
    }
    if (is.null(tb)) {
        tb <- read.table(repfile, fill = T, col.names = paste("V", 
            1:max(cl), sep = ""), as.is = T, blank.lines.skip = FALSE)
    }
    name.label <- find.and.read.table2("AGE_LENGTH_KEY", skipline = vskipline - 
        1, gyou = 1, table.property = cl, tb = tb, outfile = repfile, 
        h = FALSE, is.ss2 = TRUE, colClasses = "character")
    ALK <- find.and.read.table2("AGE_LENGTH_KEY", skipline = vskipline, 
        gyou = NULL, tb = tb, table.property = cl, outfile = repfile, 
        h = FALSE, is.ss2 = TRUE, target.line = name.label[[2]])
    ALK[[2]] <- ALK[[2]]
    colnames(ALK[[1]]) <- as.character(name.label[[1]])
    if (all == TRUE) {
        tmp <- ALK[[1]]
        tmp.line <- ALK[[2]] - nrow(ALK[[1]])
        res <- list()
        res[[1]] <- tmp
        desc <- ifelse(!is.ss2.2(repfile) && !is.ss3(repfile), 
            "SEASON:", "Seas:")
        desc.line <- which(tb[, 1] == desc)
        for (i in 1:length(desc.line)) {
            tmp <- find.and.read.table2(desc, skipline = 1, gyou = NULL, 
                tb = tb, table.property = cl, outfile = repfile, 
                h = FALSE, is.ss2 = TRUE, target.line = desc.line[i])
            res[[i]] <- tmp[[1]]
            dimnames(res[[i]]) <- dimnames(res[[1]])
            tmp.line <- tmp[[2]] - nrow(ALK[[1]])
        }
        ALK[[1]] <- res
    }
    list(ALK[[1]], ALK[[2]])
}
