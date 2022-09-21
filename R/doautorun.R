doautorun <-
function (datfile = NULL, ctlfile = NULL, process.graph = TRUE, 
    repfile = "ss2.rep", parfile = "ss2.par", namesfile = ifelse(vnumber < 
        2, "SS2names.nam", "starter.ss2"), forecastfile = "forecast.ss2", 
    max.grad = 1e+20, max.calc = c(3, 10), ask.repeat = FALSE, 
    initN.op = 1, effN.op = 2, store.repfile = TRUE, retro.year = NULL, 
    effN.arg = list(zero.intercept = TRUE, by.fleet = TRUE, conversion.criteria = 0.1), 
    kai.lim = 0.1, initN.lim = 1000, how.many = 1, vnumber = 2, 
    hess = TRUE, ss2.arg = "", ss2.arg1st = "", filename.stored = NULL, 
    adjust.cpuesd = FALSE, exclude.effNfleet = NULL, debug.mode = FALSE) 
{
    ss2.grad <- kai.tmp <- ss2.LL <- numeric()
    paste2 <- function(x, ...) {
        paste(x, sep = "", ...)
    }
    file.copy2(from = namesfile, to = paste2(namesfile, "_o"))
    tmpfunc <- function() {
        file.copy2(from = namesfile, to = paste2(namesfile, "_last"))
        file.copy2(from = paste2(namesfile, "_o"), to = namesfile)
        if (names(dev.cur()) != "null device") 
            dev.off()
    }
    on.exit(tmpfunc())
    names.obj <- names.obj.o <- read.table(namesfile, as.is = T)
    forecast.obj <- read.table(forecastfile, as.is = T)
    doforecast <- ifelse(vnumber < 2, forecast.obj[2, 1], names.obj[22, 
        1])
    if (vnumber < 2) {
    }
    else {
    }
    if (vnumber >= 2) 
        names.obj[16, 1] <- ifelse(is.null(retro.year), names.obj[16, 
            1], retro.year)
    write.table(names.obj, file = namesfile, row.names = F, col.names = F, 
        quote = FALSE)
    min.grad <- ifelse(vnumber < 2, as.numeric(names.obj[16, 
        1]), as.numeric(names.obj[15, 1]))
    if (is.null(datfile)) 
        datfile <- names.obj[1, 1]
    if (is.null(ctlfile)) 
        ctlfile <- names.obj[2, 1]
    b <- replacedat.effN(0, datfile = datfile, outfile = "tmp.dat")
    nsmp.res <- array(-100, dim = c(length(b[[1]]), max.calc[2] + 
        1, 8))
    dimnames(nsmp.res) <- list(paste("Obs", 1:length(b[[1]]), 
        sep = ""), paste("Cal", 1:(max.calc[2] + 1), sep = ""), 
        c("Original", "Replaced", "Pred_from", "Eff_op1", "Eff_op2", 
            "Eff_op3", "Nsamp", "fleet"))
    nsmp.res[, , 8] <- b$age.comp.obs$Fisheries
    nsmp.res[, 1, 1] <- b$oldsmp
    initN <- switch(initN.op, b$oldsmp, cutlimit(b$oldsmp, lim = initN.lim), 
        constsmp(b$oldsmp, lim = initN.lim), b$oldsmp * initN.lim)
    nsmp.res[, 1, 2] <- initN
    b <- replacedat.effN(initN, datfile = datfile, outfile = "ss2-init.dat")
    sd.cpue <- tapply(as.numeric(b$cpue[, 5]), factor(b$cpue[, 
        3], levels = 1:max(as.numeric(b$cpue[, 3]))), mean)
    sd.cpue <- ifelse(is.na(sd.cpue), 0, sd.cpue)
    sd.cpue.mat <- matrix(0, length(sd.cpue), max.calc[2])
    dimnames(sd.cpue.mat) <- list(1:length(sd.cpue), paste("Cal", 
        1:max.calc[2], sep = ""))
    sd.cpue.mat[, 1:ifelse(max.calc[2] < 3, max.calc[2], 3)] <- sd.cpue
    names.obj[1, 1] <- "ss2-init.dat"
    names.obj[2, 1] <- ctlfile
    if (vnumber < 2) {
        names.obj[4, 1] <- "0"
    }
    else {
        names.obj[3, 1] <- "0"
    }
    write.table(names.obj, file = namesfile, row.names = F, col.names = F, 
        quote = FALSE)
    n.ss2 <- 1
    if (ss2.arg1st == "") 
        ss2.arg1st <- ss2.arg
    if (debug.mode == FALSE) {
        doss2(how.many = how.many, ss2.arg = ss2.arg1st)
    }
    ss2.grad[n.ss2] <- read.grad(parfile)
    ss2.LL[n.ss2] <- read.like(parfile)
    if (store.repfile == TRUE) 
        mvfile(filename.stored, n.ss2)
    cat("1st calculation is finished with maximum gradient component of ", 
        ss2.grad[n.ss2], ".\n", file = "process_track.txt")
    n.ss2 <- 2
    if (vnumber < 2) {
        names.obj[4, 1] <- "1"
    }
    else {
        names.obj[3, 1] <- "1"
    }
    write.table(names.obj, file = namesfile, row.names = F, col.names = F, 
        quote = FALSE)
    if (debug.mode == FALSE) {
        doss2(how.many = how.many, ss2.arg = ss2.arg)
    }
    nsmp.res[, 2, 1] <- initN
    nsmp.res[, 2, 2] <- initN
    ss2.grad[n.ss2] <- read.grad(parfile)
    ss2.LL[n.ss2] <- read.like(parfile)
    if (store.repfile == TRUE) 
        mvfile(filename.stored, n.ss2)
    cat("2nd calculation is finished with maximum gradient component of ", 
        ss2.grad[n.ss2], ".\n", file = "process_track.txt", append = T)
    over.g <- 1
    n.ss2 <- n.ss2 + 1
    if (effN.op == 3) {
        cl0 <- count.fields(repfile, blank.lines.skip = FALSE)
        tb0 <- read.table(repfile, fill = T, col.names = paste("V", 
            1:max(cl0), sep = ""), as.is = T, blank.lines.skip = FALSE)[, 
            1:2]
        target0 <- getAgecomp.ss2(repfile, tb = tb0, cl = cl0)[[3]]
    }
    repeat {
        agecomp <- getAgecomp.ss2.1(repfile)[[1]]
        nsmp.res[, n.ss2, 4] <- agecomp$effN
        nsmp.res[, n.ss2, 6] <- ifelse(effN.op == 3, lengthdist(getAgecomp.ss2(repfile))$label$effN2, 
            -100)
        nsmp.res[, n.ss2, 7] <- agecomp$Nsamp
        if (effN.op == 2) {
            if (is.null(effN.arg$coef)) {
                effN.arg$coef <- as.list(numeric())
            }
            if (effN.arg$by.fleet == FALSE) {
                if (effN.arg$zero.intercept == TRUE) {
                  tmp <- lm(agecomp$effN ~ agecomp$Nsamp + 0)
                }
                else {
                  tmp <- lm(agecomp$effN ~ agecomp$Nsamp)
                }
                nsmp.res[, n.ss2, 5] <- tmp$fitted.values
                effN.arg$coef[[n.ss2]] <- tmp$coef
                cat("Multiplier to effN ", tmp$coef, ".\n", file = "process_track.txt", 
                  append = T)
            }
            else {
                fleets <- nsmp.res[, 1, 8]
                nfleets <- unique(fleets)
                effN.arg$coef[[n.ss2]] <- matrix(0, length(nfleets), 
                  2, dimnames = list(nfleets, c("bias", "intercept")))
                for (i in 1:length(nfleets)) {
                  if (effN.arg$zero.intercept == TRUE) {
                    tmp <- lm(agecomp$effN[fleets == nfleets[i]] ~ 
                      agecomp$Nsamp[fleets == nfleets[i]] + 0)
                    effN.arg$coef[[n.ss2]][i, 1] <- tmp$coef
                  }
                  else {
                    tmp <- lm(agecomp$effN[fleets == nfleets[i]] ~ 
                      agecomp$Nsamp[fleets == nfleets[i]])
                    effN.arg$coef[[n.ss2]][i, ] <- c(tmp$coef[2], 
                      tmp$coef[1])
                  }
                  nsmp.res[fleets == nfleets[i], n.ss2, 5] <- tmp$fitted.values
                }
                cat("Multiplier to effN ", round(effN.arg$coef[[n.ss2]], 
                  4), ".\n", file = "process_track.txt", append = T)
            }
        }
        if (n.ss2 == 3) {
            effN <- switch(effN.op, nsmp.res[, n.ss2, 4], nsmp.res[, 
                n.ss2, 5], nsmp.res[, n.ss2, 6])
            datfile2 <- "ss2-init.dat"
        }
        else {
            effN <- switch(effN.op, nsmp.res[, n.ss2, 4], nsmp.res[, 
                n.ss2, 5], nsmp.res[, n.ss2, 6])
            datfile2 <- "ss2-new.dat"
        }
        if (n.ss2 > 3 && adjust.cpuesd == TRUE) {
            sd.offset <- replace.sd.offset(ctrfile = names.obj[2, 
                1], repfile = repfile, newctl = "control_new.ctl", 
                def.sd = sd.cpue, vnumber = vnumber)
            names.obj[2, 1] <- "control_new.ctl"
            sd.cpue.mat[, n.ss2] <- sd.offset
            write.table(names.obj, file = namesfile, row.names = F, 
                col.names = F, quote = FALSE)
        }
        b <- replacedat.effN(effN, datfile = datfile2, outfile = "ss2-new.dat", 
            exclude.effNfleet = exclude.effNfleet)
        nsmp.res[, n.ss2, 1] <- b$oldsmp
        nsmp.res[, n.ss2, 2] <- b$newsmp
        nsmp.res[, n.ss2, 3] <- lm(b$newsmp ~ b$oldsmp)$fitted.values
        kai.tmp[n.ss2] <- sum((b$newsmp - nsmp.res[, n.ss2, 3])^2/nsmp.res[, 
            n.ss2, 3])/length(b$newsmp)
        names.obj[1, 1] <- "ss2-new.dat"
        write.table(names.obj, file = namesfile, row.names = F, 
            col.names = F, quote = FALSE)
        if (debug.mode == FALSE) {
            doss2(how.many = how.many, ss2.arg = ss2.arg)
        }
        ss2.grad[n.ss2] <- read.grad(parfile)
        ss2.LL[n.ss2] <- read.like(parfile)
        if (store.repfile == TRUE) 
            mvfile(filename.stored, n.ss2)
        cat(n.ss2, "th calculation is finished with maximum gradient component of ", 
            ss2.grad[n.ss2], ".\n", file = "process_track.txt", 
            append = T)
        nearlast.like <- read.like(parfile)
        answer2 <- NULL
        is.finished <- FALSE
        if (ask.repeat == TRUE && max.calc[2] == n.ss2 && (ss2.grad[n.ss2] > 
            min.grad | kai.tmp[n.ss2] > kai.lim)) {
            cat(" ** Max grad: ", ss2.grad[n.ss2], "\n ** Sum of kai squared: ", 
                kai.tmp[n.ss2], "\n")
            if (!is.null(effN.arg$coef)) {
                cat(" ** multiplier for effective sample size: ", 
                  round(effN.arg$coef[[n.ss2]][, 1], 3), "\n")
            }
            cat("The model can't be judged to be converged. Do you want to replace effective sample size? Enter Yes (Y) or No (N)  ")
            answer1 <- readline()
            YorN <- substr(answer1, 1, 1) == "Y" | substr(answer1, 
                1, 1) == "y"
            if (!YorN) {
                cat("Do you want to run ss2 without replacing effective sample size until converging? Enter Yes (Y) or No (N)  ")
                answer2 <- readline()
                YorN2 <- substr(answer2, 1, 1) == "Y" | substr(answer2, 
                  1, 1) == "y"
                if (YorN2) {
                  is.finished <- TRUE
                  cat("How many? Enter the number.")
                  last.run.number <- floor(as.numeric(readline()))
                  while (is.na(last.run.number)) {
                    cat("Please enter numerial value!")
                    last.run.number <- floor(as.numeric(readline()))
                  }
                }
                else {
                  NULL
                }
            }
            else {
                cat("How many? Enter the number.")
                add.calc <- floor(as.numeric(readline()))
                while (is.na(add.calc)) {
                  cat("Please enter numerial value!")
                  add.calc <- floor(as.numeric(readline()))
                }
                max.calc[2] <- max.calc[2] + add.calc
                tmp <- nsmp.res
                nsmp.res <- array(-100, dim = c(length(b[[1]]), 
                  max.calc[2] + 1, 8))
                dimnames(nsmp.res) <- list(paste("Obs", 1:length(b[[1]]), 
                  sep = ""), paste("Cal", 1:(max.calc[2] + 1), 
                  sep = ""), c("Original", "Replaced", "Pred_from", 
                  "Eff_op1", "Eff_op2", "Eff_op3", "Nsamp", "fleet"))
                nsmp.res[, 1:n.ss2, ] <- tmp[, 1:n.ss2, ]
                sd.cpue.mat <- cbind(sd.cpue.mat, matrix(0, nrow(sd.cpue.mat), 
                  ncol(sd.cpue.mat)))
                colnames(sd.cpue.mat) <- paste("Cal", 1:ncol(sd.cpue.mat))
            }
        }
        if ((ss2.grad[n.ss2] < min.grad && kai.tmp[n.ss2] < kai.lim) | 
            max.calc[2] == n.ss2 | is.finished == TRUE) {
            names(nsmp.res)[n.ss2] <- "Last_est"
            a <- 0
            last.like <- 0
            if (hess == TRUE) {
                if (!is.null(answer2)) {
                  cat("YorN2=", YorN2, "\n")
                  if (YorN2) {
                    last.run.tmp <- 1
                    while (ss2.grad[n.ss2] > min.grad && last.run.tmp <= 
                      last.run.number) {
                      curCond <- (ss2.grad[n.ss2] > min.grad && 
                        last.run.tmp <= last.run.number)
                      cat("currentCondition is ", curCond, " gradient is ", 
                        ss2.grad[n.ss2], " last.run.tmp is ", 
                        last.run.tmp, "\n", file = "process_track.txt", 
                        append = T)
                      n.ss2 <- n.ss2 + 1
                      if (debug.mode == FALSE) 
                        doss2(ss2.arg = ss2.arg)
                      last.run.tmp <- last.run.tmp + 1
                      ss2.grad[n.ss2] <- read.grad(parfile)
                      cat("Calculation without replacing effective sample size is finished with maximum gradient component of ", 
                        ss2.grad[n.ss2], ".\n", file = "process_track.txt", 
                        append = T)
                    }
                  }
                }
                if (debug.mode == FALSE) 
                  doss2.withhess(ss2.arg = ss2.arg)
            }
            last.grad <- read.grad(parfile)
            cat("Last run was conducted for estimating hessian matrix.  The last value of max gradient is ", 
                read.grad(parfile), ".\n", file = "process_track.txt", 
                append = T)
            if (!is.null(retro.year)) 
                cat("CHECK: The number of retrospective year was externally replaced to", 
                  retro.year, "from", names.obj.o[16, 1], "by this R function.\n")
            if (n.ss2 >= max.calc[2]) 
                cat("The last max grad is", last.grad, " and sum of kai squared is", 
                  kai.tmp[n.ss2], ", \n but the number of calculation exceeds", 
                  max.calc[2], "times. Then the calculation was forced to be finished.\n            The calculation might not be converged. Please check the condition!\n")
            else cat("The gradient value is smaller than ", min.grad, 
                " and kai squared statics also smaller than", 
                kai.lim, " (or number of calculation exceeds max.calc[2]),", 
                "then the calculation is finished successflly. Congraturation!\n")
            break
        }
        if (ss2.grad[n.ss2] > max.grad) {
            over.g <- over.g + 1
            if (over.g > max.calc[1]) {
                cat("The gradient value is ", ss2.grad[n.ss2], 
                  ", which exceeded ", max.grad, " ", max.calc[1], 
                  " times. Because this calculation would not be converged, the calculation is forced to be stopped.\n")
                if (!is.null(retro.year)) 
                  cat("CHECK: The number of retrospective year was externally replaced from", 
                    retro.year, "from", names.obj.o[16, 1], "by this R function.")
                break
            }
        }
        n.ss2 <- n.ss2 + 1
    }
    agecomp <- getAgecomp.ss2.1(repfile)[[1]]
    nsmp.res[, dim(nsmp.res)[[2]], 4] <- agecomp$effN
    nsmp.res[, dim(nsmp.res)[[2]], 6] <- ifelse(effN.op == 3, 
        lengthdist(getAgecomp.ss2(repfile))$label$effN2, -100)
    nsmp.res[, dim(nsmp.res)[[2]], 5] <- lm(agecomp$effN ~ agecomp$Nsamp)$fitted.values
    nsmp.res[, dim(nsmp.res)[[2]], 7] <- agecomp$Nsamp
    write.table(cbind(ss2.grad, kai.tmp, ss2.LL), file = "eff_smp_track.txt")
    for (i in 1:dim(nsmp.res)[[2]]) {
        cat(i, "th calc:\n", file = "eff_smp_track.txt", append = T)
        write.table(nsmp.res[, i, ], file = "eff_smp_track.txt", 
            append = T, row.names = FALSE)
    }
    if (!is.null(filename.stored)) {
        if (substr(filename.stored[1], nchar(filename.stored[1]), 
            nchar(filename.stored[1])) != "/") 
            filename.stored[1] <- paste2(filename.stored[1], 
                "/")
        if (!file.exists(filename.stored[1])) 
            dir.create(filename.stored[1])
        file.copy2(from = "ss2.rep", to = paste2(filename.stored[1], 
            "/ss2", filename.stored[2], ".rep"))
        file.copy2(from = "ss2.par", to = paste2(filename.stored[1], 
            "/ss2", filename.stored[2], ".par"))
        file.copy2(from = "eff_smp_track.txt", to = paste2(filename.stored[1], 
            "/eff_smp_track", filename.stored[2], ".txt"))
        file.copy2(from = "process_track.txt", to = paste2(filename.stored[1], 
            "/process_track", filename.stored[2], ".txt"))
    }
    if (effN.op != 2) {
        res <- list(grad = ss2.grad, nsmp.res = nsmp.res, sd.cpue.mat = sd.cpue.mat, 
            LL = ss2.LL)
    }
    else {
        res <- list(grad = ss2.grad, nsmp.res = nsmp.res, sd.cpue.mat = sd.cpue.mat, 
            LL = ss2.LL, effN.coef = effN.arg$coef)
    }
    if (process.graph == TRUE && debug.mode == FALSE) {
        plot.processgraph(res)
    }
    return(invisible(res))
}
