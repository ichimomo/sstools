branov.eq <-
function (x, n.age, naat.pred.vec, select.para, waa, nmaa, CHS) 
{
    s <- 0
    for (i in c(1:n.age)) {
        s <- s + (naat.pred.vec[i] * waa[i] * select.para[i] * 
            x)/(select.para[i] * x + nmaa[i]) * (1 - exp(-select.para[i] * 
            x - nmaa[i]))
    }
    (s/1000 - CHS)^2
}
