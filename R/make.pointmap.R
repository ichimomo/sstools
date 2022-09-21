make.pointmap <-
function (x2, cex = 1.5, limit = c(0, 0.2, 0.5, 1, 5, 10)) 
{
    library(RColorBrewer)
    col.tmp <- c(2, 3, 4, 6, 8, 9)
    xtmp <- as.numeric(x2[3, ] > limit[1])
    xtmp <- xtmp + as.numeric(x2[3, ] >= limit[2])
    xtmp <- xtmp + as.numeric(x2[3, ] >= limit[3])
    xtmp <- xtmp + as.numeric(x2[3, ] >= limit[4])
    xtmp <- xtmp + as.numeric(x2[3, ] >= limit[5])
    xtmp <- xtmp + as.numeric(x2[3, ] >= limit[6])
    tmp <- x2[3, ] != 0 & !is.na(x2[3, ]) & x2[3, ] != Inf
    points(x2[2, tmp] + 0.5, x2[1, tmp] + 0.5, col = brewer.pal(9, 
        "Reds")[col.tmp[xtmp[tmp]]], pch = 15, cex = cex)
    tmp <- x2[3, ] == 0 & !is.na(x2[3, ]) & x2[3, ] != Inf
    points(x2[2, tmp] + 0.5, x2[1, tmp] + 0.5, pch = 4, cex = cex, 
        col = "skyblue")
}
