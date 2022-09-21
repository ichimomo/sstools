read.datfile <-
function (datfile = "ss2.dat", outfile = "ss_new.dat", cpue.replace = NULL, 
    catch.replace = NULL, length.replace = NULL) 
{
    cf <- count.fields(datfile, comment = "", blank.lines.skip = T)
    a <- read.table(datfile, fill = T, col.names = paste("V", 
        1:max(cf), sep = ""), as.is = T)
    a <- as.data.frame(a)
    nyear <- as.numeric(a[2, 1]) - as.numeric(a[1, 1]) + 1
    y1 <- nyear * as.numeric(a[3, 1]) + 18
    ncpue <- as.numeric(a[y1, 1])
    cpue <- a[cpue.range <- ((y1 + 1):(y1 + ncpue)), ]
    ndiscard <- as.numeric(a[ncpue + y1 + 2, 1])
    nmeanwt <- as.numeric(a[ncpue + y1 + 3 + ndiscard, 1])
    nobs <- as.numeric(a[y2 <- ncpue + y1 + 10 + ndiscard + nmeanwt, 
        1])
    length.label <- a[y2 - 1, ]
    len.comp.obs <- a[length.range <- ((y2 + 1):(y2 + nobs)), 
        ]
    catch <- a[catch.range <- 18:(y1 - 1), ]
    if (!is.null(length.replace)) {
        length.new <- matrix(NA, nrow(length.replace), ncol(length.replace))
        length.new[, 1:ncol(length.replace)] <- as.matrix(length.replace)
        a <- rbind(a[1:(min(length.range) - 1), ], length.new, 
            a[(max(length.range) + 1):nrow(a), ])
        a[y2, 1] <- nrow(length.new)
    }
    if (!is.null(cpue.replace)) {
        cpue.new <- matrix(NA, nrow(cpue.replace), ncol(cpue))
        cpue.new[, 1:ncol(cpue.replace)] <- as.matrix(cpue.replace)
        a <- rbind(a[1:(min(cpue.range) - 1), ], cpue.new, a[(max(cpue.range) + 
            1):nrow(a), ])
        a[y1, 1] <- nrow(cpue.new)
    }
    if (!is.null(catch.replace)) {
        catch.new <- matrix(NA, nrow(catch.replace), ncol(catch))
        catch.new[, 1:ncol(catch.replace)] <- as.matrix(catch.replace)
        a <- rbind(a[1:(min(catch.range) - 1), ], catch.new, 
            a[(max(catch.range) + 1):nrow(a), ])
        a[17, 1] <- nrow(catch.new)
        a[2, 1] <- max(floor(as.numeric(rownames(catch.replace))))
    }
    cat("# This file is the dat file, in which some catch data were automatically replaced into zero\n", 
        file = outfile, append = F)
    write.table(a, na = "", file = outfile, row.names = F, col.names = F, 
        append = T, quote = FALSE)
    invisible(list(catch = catch, size = len.comp.obs, cpue = cpue, 
        length.label = length.label[!is.na(length.label)]))
}
