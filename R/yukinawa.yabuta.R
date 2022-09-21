yukinawa.yabuta <-
function (age) 
{
    len <- 320.5 * (1 - exp(-0.1035 * age - 0.0728))
    len
}
