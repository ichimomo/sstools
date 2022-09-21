pictex.template <-
function (picfile = "test.tex", tempfile = "template.tex") 
{
    cat("\\documentclass[a4paper]{jarticle}\n\\usepackage{pictex}\n\\usepackage{graphics}\n\\begin{document}\n\\input{", 
        file = tempfile)
    cat(picfile, file = tempfile, append = T)
    cat("}\n\\end{document}", file = tempfile, append = T)
}
