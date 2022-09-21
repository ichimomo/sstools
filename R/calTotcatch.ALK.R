calTotcatch.ALK <-
function (repfile, qt = 4) 
{
    caa <- getCAA.ss2(repfile)
    WatL <- getWatL.ss2(repfile)[[1]]
    ALK <- getALK.ss2(repfile = repfile, all = TRUE, qt = qt)[[1]]
    label <- as.numeric(unfactor(ALK[[1]][, 1]))
    for (i in 1:qt) {
        ALK[[i]] <- ALK[[i]][!is.na(label), -1]
    }
    label <- label[!is.na(label)]
    for (i in 1:qt) {
        ALK[[i]] <- ALK[[i]][order(label), ]
        rownames(ALK[[i]]) <- label[order(label)]
    }
    label <- label[order(label)]
    wcaa.array <- caa$caa.array
    dc <- dim(wcaa.array)
    if (sum(WatL$low != label) == 0) {
        for (i in 1:dc[3]) {
            for (j in 1:dc[2]) {
                for (k in 1:dc[1]) {
                  wcaa.array[k, j, i] <- sum(caa$caa.array[k, 
                    j, i] * ALK[[qtback(dimnames(wcaa.array)[[1]][k])]][, 
                    j] * WatL$Wt)
                }
            }
        }
    }
    else {
        cat("Length of label and WatL is different !!!!")
    }
    return(list(wcaa.array = wcaa.array, ALK = ALK, WatL = WatL))
}
