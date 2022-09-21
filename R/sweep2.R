sweep2 <-
function (x) 
{
    sweep(x, 2, apply(x, 2, sum), FUN = "/")
}
