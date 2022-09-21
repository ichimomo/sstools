do.bootstrap.ss3 <-
function (namesfile = "starter.ss", control.boot = "control_boot.SS", 
    ss3.arg = "-nox -nohess", grad.criteria = 0.1, nboot = 300, 
    only.readdat = FALSE, use.parfile = TRUE, is.plot = F, debug.mode = F, 
    max.calc = 5, additive.num = 0, save.faa = FALSE) 
{
    file.copy2(from = namesfile, to = paste2(namesfile, "_o"))
    exit.function <- function() {
        file.copy2(from = namesfile, to = paste2(namesfile, "_last"))
        file.copy2(from = paste2(namesfile, "_o"), to = namesfile)
        file.copy2(from = "ss3-org.par", to = "ss3.par")
        save(grad.rec, file = "grad.rec.R")
    }
    on.exit(exit.function())
    names.obj <- names.obj.o <- read.table(namesfile, as.is = T)
    if (is.null(nboot)) {
        nboot <- as.numeric(names.obj[11, 1])
    }
    if (nboot == 0) 
        stop("Please input a figure more than 0 in the 11th line")
    grad.rec <- data.frame(grad = rep(0, nboot + 1), ncalc = rep(0, 
        nboot + 1))
    grad.tmp <- 10
    if (only.readdat == FALSE) {
        names.obj[11, 1] <- nboot
        names.obj[3, 1] <- 0
        write.table("#NuStarter.SS2", file = namesfile, row.names = F, 
            col.names = F, quote = FALSE)
        write.table(names.obj, file = namesfile, row.names = F, 
            col.names = F, quote = FALSE, append = T)
        grad.tmp <- 10
        s <- 1
        while (grad.tmp > grad.criteria && s < max.calc) {
            if (s > 1) {
                names.obj[3, 1] <- 1
                write.table("#NuStarter.SS2", file = namesfile, 
                  row.names = F, col.names = F, quote = FALSE)
                write.table(names.obj, file = namesfile, row.names = F, 
                  col.names = F, quote = FALSE, append = T)
            }
            if (debug.mode == F) {
                doss3(ss3.arg = ss3.arg)
            }
            grad.tmp <- read.grad(parfile = "ss3.par")
            s <- s + 1
        }
        grad.rec$grad[1] <- grad.tmp
        grad.rec$ncalc[1] <- s - 1
        file.copy2(from = "Report.sso", to = "Report_org.sso")
        file.copy2(from = "ss3.par", to = "ss3-org.par")
        file.copy2(from = "data.ss_new", to = "data.ss_new_org")
    }
    file.copy2(from = "data.ss_new", to = "data.ss_new_org")
    cf <- count.fields("data.ss_new", comment.char = "")
    a <- read.table("data.ss_new", fill = T, col.names = paste("V", 
        1:max(cf), sep = ""), comment.char = "")
    cut.point <- c(2, which(a[, 1] == 999))
    names.obj[1, 1] <- "data-boot.ss"
    names.obj[2, 1] <- control.boot
    names.obj[3, 1] <- as.numeric(use.parfile)
    names.obj[11, 1] <- 0
    write.table("#NuStarter.SS2", file = namesfile, row.names = F, 
        col.names = F, quote = FALSE)
    write.table(names.obj, file = namesfile, row.names = F, col.names = F, 
        quote = FALSE, append = T)
    for (i in 1:nboot) {
        write.table(a[(cut.point[i] + 1):cut.point[i + 1], ], 
            na = "", file = "data-boot.ss", row.names = F, col.names = F, 
            quote = FALSE)
        file.copy2(from = "ss3-org.par", to = "ss3.par")
        file.copy2(from = "data-boot.ss", to = paste("data-boot", 
            sprintf("%03.0f", i + additive.num), ".ss", sep = ""))
        s <- 1
        grad.tmp <- 10
        while (grad.tmp > grad.criteria && s < max.calc) {
            if (debug.mode == F) {
                doss3(ss3.arg = ss3.arg)
            }
            grad.tmp <- read.grad(parfile = "ss3.par")
            s <- s + 1
        }
        grad.rec$grad[i + 1] <- grad.tmp
        grad.rec$ncalc[i + 1] <- s - 1
        file.copy2(from = "Report.sso", to = paste2("Report_b", 
            sprintf("%03.0f", i + additive.num), ".sso"))
        file.copy2(from = "CompReport.sso", to = paste("CompReport", 
            sprintf("%03.0f", i + additive.num), ".sso", sep = ""))
        file.copy2(from = "ss3.par", to = paste("ss3_b", sprintf("%03.0f", 
            i + additive.num), ".par", sep = ""))
    }
    if (is.plot == T) {
        tmp <- getBabs.ss2("ss3-org.rep")
        biom <- tmp[[1]]
        biom.target <- tmp[[2]]
        biom.list <- as.list(1:nboot)
        for (i in 1:nboot) {
            biom.list[[i]] <- getBabs.ss2(paste2("Report", i + 
                additive.num, ".sso"), target.line = biom.target - 
                10 - nrow(biom))[[1]]
        }
        plotBSR(biom, biom.list)
    }
    save(grad.rec, file = "grad.rec.R")
    if (save.faa == TRUE) {
        for (i in 1:nboot) {
            faa <- calFAA.ss2(paste2("Report_b", sprintf("%03.0f", 
                i + additive.num), ".SSO"))
            save(faa, file = paste2("faa", sprintf("%03.0f", 
                i + additive.num), ".R"))
        }
    }
    return(grad.rec)
}
