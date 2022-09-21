calc.B0 <-
function (waa, nmaa, R.num) 
{
    sum(NperR(nmaa, rep(0, length(nmaa)), 0) * waa)/1000 * R.num
}
