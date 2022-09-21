make.pictexfig <-
function (picfile = "test.tex", tempfile = "template.tex", graphfile = "test.ps") 
{
    system(paste("platex ", tempfile))
    tmp <- strsplit(tempfile, "\\.")
    system(paste("pdvips ", tmp[[1]][1], ".dvi", " -o ", graphfile, 
        sep = ""))
    system(paste("ggv ", graphfile, "&"))
}
