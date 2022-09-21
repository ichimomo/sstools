\name{sstools-package}
\alias{sstools-package}
\alias{sstools}
\docType{package}
\title{
Functions to read and plot results of SS. 
}
\description{
Functions to read and plot results of SS.  Main funtion is allplot.ss.
}
\details{
\tabular{ll}{
Package: \tab sstools\cr
Type: \tab Package\cr
Version: \tab 1.5\cr
Date: \tab 2011-04-22\cr
License: \tab ichimomo@fra.affrc.go.jp\cr
LazyLoad: \tab yes\cr
}

}
\author{
Momoko Ichinokawa

Maintainer: Momoko Ichinokawa <ichimomo@fra.affrc.go.jp>
}
\references{

}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
 http://cse.fra.affrc.go.jp/ichimomo/Tuna/sstools.html
}
\examples{
library(sstools)
# No options
allplot()

# overplots of multiple results
allplot(repfile=c("Report1.SSO","Report2.SSO","Report3.SSO"),
        compfiles=c("CompReport1.SSO","CompReport2.SSO","CompReport3.SSO"))

}
