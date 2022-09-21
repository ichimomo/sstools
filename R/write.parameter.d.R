write.parameter.d <-
function (zahyou, para.list, kiri = 1, moji = "-") 
{
    n <- length(para.list)
    moji <- NULL
    for (i in c(1:n)) {
        moji <- paste(moji, names(para.list)[i], ": ", paste.new(round(para.list[[i]][1:length(para.list[[i]])], 
            kiri)), "\n")
    }
    text(zahyou[1], zahyou[2], as.character(moji), pos = 4)
}
