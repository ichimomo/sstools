solve.baranov <-
function (nage, naat.pre, select.para, waa, nmaa, CHS) 
{
    .C("slvbaranov", arg1 = as.integer(nage), arg2 = as.double(naat.pre), 
        arg3 = as.double(select.para), arg4 = as.double(waa), 
        arg5 = as.double(nmaa), arg6 = as.double(CHS), arg7 = double(1))$arg7
}
