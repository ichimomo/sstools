\name{pictex.template}
\alias{pictex.template}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pictex.template(picfile = "test.tex", tempfile = "template.tex")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{picfile}{
%%     ~~Describe \code{picfile} here~~
}
  \item{tempfile}{
%%     ~~Describe \code{tempfile} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(picfile="test.tex",tempfile="template.tex"){
  cat("\\documentclass[a4paper]{jarticle}
\\usepackage{pictex}
\\usepackage{graphics}
\\begin{document}
\\input{",
      file=tempfile)
  cat(picfile,file=tempfile,append=T)
  cat("}
\\end{document}",file=tempfile,append=T)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
