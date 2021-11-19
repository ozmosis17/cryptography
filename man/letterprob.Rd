\name{letterprob}
\alias{letterprob}
\docType{data}
\title{
Likelihood matrix of all two-letter combinations
}
\description{
  This matrix gives the likelihood of any two-character combinations appearing
  in the English language, including spaces. Row/column 1 represents spaces,
  while rows/columns 2-27 represent letters. The character that appears first
  in the combination is represented by the row.
}
\usage{data("letterprob")}
\format{
  The format is:
 num [1:27, 1:27] 5.57e-06 7.84e-02 6.77e-03 9.49e-03 6.48e-01 ...
}
\source{
  Rice University CAAM 210
}
\examples{
data(letterprob)
}
\keyword{datasets}
