branov.eq.pre3 <-
function (x, n.age, naat.pred.vec, select.para, waa, nmaa, partial.catch) 
{
    tc <- (naat.pred.vec * waa * partial.catch * x)/(select.para * 
        x + nmaa) * (1 - exp(-select.para * x - nmaa))
    tc/1000
}
