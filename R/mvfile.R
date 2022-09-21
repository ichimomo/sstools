mvfile <-
function (filename.stored, n.ss2) 
{
    if (.Platform$OS.type == "unix") {
        system(paste("cp ss2.rep ", "ss2", filename.stored[2], 
            "-", n.ss2, ".rep", sep = ""))
        system(paste("mv ss2", filename.stored[2], "-", n.ss2, 
            ".rep ", filename.stored[1], sep = ""))
        system(paste("cp ss2.par ", "ss2", filename.stored[2], 
            "-", n.ss2, ".par", sep = ""))
        system(paste("mv ss2", filename.stored[2], "-", n.ss2, 
            ".par ", filename.stored[1], sep = ""))
    }
    else {
        shell(paste("copy ss2.rep ", "ss2", filename.stored[2], 
            "-", n.ss2, ".rep", sep = ""))
        shell(paste("move ss2", filename.stored[2], "-", n.ss2, 
            ".rep ", filename.stored[1], sep = ""))
        shell(paste("copy ss2.par ", "ss2", filename.stored[2], 
            "-", n.ss2, ".par", sep = ""))
        shell(paste("move ss2", filename.stored[2], "-", n.ss2, 
            ".par ", filename.stored[1], sep = ""))
    }
}
