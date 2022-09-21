write.parameter <-
function (zahyou, para.list, kiri = 1, moji = "-") 
{
    n <- length(para.list)
    moji <- NULL
    for (i in c(1:n)) {
        ifelse(length(para.list[[i]]) == 1, moji <- paste(moji, 
            names(para.list)[i], ": ", paste(para.list[[i]]), 
            "\n"), moji <- paste(moji, names(para.list)[i], ": ", 
            paste(round(min(para.list[[i]]), kiri)), moji, paste(round(max(para.list[[i]]), 
                kiri)), "\n"))
    }
    text(zahyou[1], zahyou[2], as.character(moji), pos = 4)
}
