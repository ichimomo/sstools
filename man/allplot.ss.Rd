\name{allplot.ss}
\alias{allplot.ss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
allplot.ss
}
\description{
To plot many diagnostics using outputs from Stock Synthesis.  
}
\usage{
allplot.ss(repfile = "Report.sso", outfile = "out", is.plot.cohortSSB = TRUE, dev.type = "pdf", len.residual.plot = F, agecomp.on = FALSE, length.on = TRUE, aselect.on = F, select.on = TRUE, col.var = NULL, lty.var = NULL, diff.target = 100, lwd.var = NULL, true.value = NULL, repfile.legend = NULL, compfile = "CompReport.sso", refpoint.year = NULL, datfile = NULL, plot.resid = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{repfile}{
A vector to specify Report file from SS.  Multiple files can be
specified for overplots.  
}
  \item{outfile}{
Name of output file.  Extension should be omitted.  
}
  \item{is.plot.cohortSSB}{
Option to plot SSB by year class.  
}
  \item{dev.type}{
 Output graphic type such as 'pdf','postscript' or 'x11'.  
  }
  \item{len.residual.plot}{
If true, length residuals are plotted.  Caution: this increases file
size very much.  
}
  \item{agecomp.on}{
If true, fits to the age composition are shown.  
}
\item{length.on}{
If true, diagnostics on length composition are shown.  
}
  \item{aselect.on}{
If true, age selectivity is shown instead of length selectivity.  
}
\item{select.on}{
If true, length selectivity is shown instead of age selectivity.    
}
  \item{col.var}{
Color valiation of lines when plotting multiple results.
}
\item{lty.var}{
Line type valiation of lines when plotting multiple results.
%%     ~~Describe \code{lty.var} here~~
}

  \item{lwd.var}{
Line width valiation of lines when plotting multiple results.
}
  \item{true.value}{
Options not for use.  
}
  \item{repfile.legend}{
Legend text for explaining report files.  length(repfile.legend) should
be same as length(repfile).  
}
  \item{compfile}{
Name of composition file.  length(compfile) should be same as
length(repfile).  
}
\item{refpoint.year}{
Options only for someone having functions to calculate reference points.   
}
  \item{datfile}{
Name of datfile.  If this is specified, fleet names are derived from
this dat file.  
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Momoko Ichinokawa (ichimomo@fra.affrc.go.jp)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

}
\examples{
# No options
allplot()

# overplots of multiple results
allplot(repfile=c("Report1.SSO","Report2.SSO","Report3.SSO"),
        compfiles=c("CompReport1.SSO","CompReport2.SSO","CompReport3.SSO"))

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
