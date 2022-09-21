replace.sd.offset <-
function (ctrfile, repfile, newctl = "control_new.ctl", def.sd = 0.2, 
    replace.sd = c(T, T, T, T, T, T), vnumber = 1) 
{
    b <- getCPUE.ss2(repfile, target.line = 11, vnumber = vnumber)[[3]]
    cl <- count.fields(ctrfile, comment = "#", blank.lines.skip = T)
    a <- read.table(ctrfile, col.names = 1:max(cl), fill = T)
    nline <- ifelse(vnumber < 2, 20, 22)
    a[nrow(a) - nline, 1:length(def.sd)] <- b$r.m.s.e - def.sd
    write.table(a, file = newctl, na = "", row.names = F, col.names = F, 
        quote = FALSE)
    return(b$r.m.s.e)
}
