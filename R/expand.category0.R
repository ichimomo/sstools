expand.category0 <-
function (num, category, category.name) 
{
    res <- data.frame(category = 0, num = 0)
    for (i in 1:length(num)) {
        res <- rbind(res, data.frame(category = category.name[i], 
            num = rep(category[i], num[i])))
    }
    res[-1, ]
}
