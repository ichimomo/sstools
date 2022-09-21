inv.logit <-
function (ip) 
{
    exp(ip)/(1 + exp(ip))
}
