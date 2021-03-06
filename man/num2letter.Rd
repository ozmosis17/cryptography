% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{num2letter}
\alias{num2letter}
\title{Converts number to letter}
\usage{
num2letter(n)
}
\arguments{
\item{n}{A vector of numbers from 1 to 27}
}
\value{
A lowercase letter or `.
}
\description{
Converts a number 2-27 to its corresponding letter, and converts 1 to "`"
}
\examples{
num2letter(c(1,2,3,4,27))
}
