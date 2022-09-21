plot.processgraph <-
function (res, smp.lim = NULL, cpue.lim = NULL, dev.type = "pdf") 
{
    nsmp.res <- res$nsmp.res
    sd.cpue.mat <- res$sd.cpue.mat
    LL <- res$LL
    ncalc <- (dim(nsmp.res)[[2]])
    if (dev.type == "pdf") {
        tmpfunc <- function() {
            if (names(dev.cur()) != "null device") 
                dev.off()
        }
        on.exit(tmpfunc())
        pdf(file = "process_track.pdf", paper = "a4", height = 9)
    }
    set.mypar()
    par(mfrow = c(3, 1), mar = c(3, 3, 1, 1))
    b <- nsmp.res[, 3:ncalc, 2]
    b <- as.data.frame(b)
    boxplot(b, ylab = "Effective sample size", ylim = smp.lim)
    matplot(t(sd.cpue.mat), type = "b", col = 1, lty = 1, xlab = "N of calculation", 
        ylab = "SD of CPUE", ylim = cpue.lim)
    plot(LL, type = "b", xlab = "N of calculation", ylab = "Negative log LL")
    fn <- sort(unique(nsmp.res[, 1, 8]))
    par(mfrow = c(5, 2), mar = c(3, 3, 2, 1))
    for (i in fn) {
        boxplot(as.data.frame(nsmp.res[nsmp.res[, 1, 8] == i, 
            3:ncalc, 2]), ylab = paste("ESS by fleet"), ylim = smp.lim)
        title(paste("fleet", i), line = 1)
    }
    if (dev.type == "pdf") {
        dev.off()
    }
}
