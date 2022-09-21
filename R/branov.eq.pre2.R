branov.eq.pre2 <-
function (x, n.age, naat.pred.vec, select.para, waa, nmaa) 
{
    tc <- (naat.pred.vec * waa * select.para * x)/(select.para * 
        x + nmaa) * (1 - exp(-select.para * x - nmaa))
    tc/1000
}
