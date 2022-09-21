count.next.line <-
function (current.line, skip.line, table.property) 
{
    k <- current.line + skip.line
    while (table.property[k] != 0 && k <= length(table.property)) {
        k <- k + 1
    }
    k - 1
}
