cor.dot <-
function (cor.res, filename = "document/temp.dot", under.value = 0.5) 
{
    cat("digraph \"foodweb.dhp\" { \n size=\"7,7\"; \n rankdir=\"LR\";\n node [fontname=\"Helvetica\" fontsize=14 shape=ellipse]; \n edge [fontname=\"Helvetica\" fontsize=12]; \n center=1;\n ", 
        file = filename)
    n <- dim(cor.res)[1]
    cat(n)
    for (i in c(1:n)) {
        cat(colnames(cor.res)[i], "\n", file = filename, append = TRUE)
        for (j in c(1:n)) {
            if (abs(cor.res[i, j]) > under.value && i > j) 
                cat(colnames(cor.res)[i], "->", colnames(cor.res)[j], 
                  "[label=", round(cor.res[i, j]), "]; \n", file = filename, 
                  append = TRUE)
        }
    }
    cat("}", file = filename, append = TRUE)
}
