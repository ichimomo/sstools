count.next.line2 <-
function (current.line, skip.line, table.property) 
{
    k <- k0 <- current.line + skip.line
    k <- k + 1
    while (table.property[k] == table.property[k0] & k <= length(table.property)) {
        k <- k + 1
    }
    k - 1
}
