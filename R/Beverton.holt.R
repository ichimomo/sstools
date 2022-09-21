Beverton.holt <-
function (SSB, coef, st = TRUE, B0 = 60000) 
{
    recruit <- 0
    if (st == FALSE) {
        recruit <- coef[1] * SSB/(coef[2] + SSB)
    }
    else {
        coef.temp <- Beverton.holt.revised2(coef, B0 = B0)
        recruit <- coef.temp[1] * SSB/(coef.temp[2] + SSB)
    }
    recruit
}
