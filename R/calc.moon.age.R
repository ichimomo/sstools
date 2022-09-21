calc.moon.age <-
function (yr, mon, day) 
{
    library(date)
    time <- (as.numeric(mdy.date(mon, day, yr)) - as.numeric(mdy.date(1, 
        1, 1970)))
    cyc <- 29.530589
    jul <- 2440587.5 + time
    k <- floor((jul - 2451550.09765)/cyc)
    t <- k/1236.85
    sg <- 2451550.09765 + cyc * k + 0.0001337 * t * t - 0.4072 * 
        sin((201.5643 + 385.8169 * k) * pi/180) + 0.17241 * sin((2.5534 + 
        29.1054 * k) * pi/180)
    moon <- jul - sg
    moonage <- floor(moon * 100)/100
    moonage
}
