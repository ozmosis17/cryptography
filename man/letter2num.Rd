% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{letter2num}
\alias{letter2num}
\title{Convert letter to number}
\usage{
letter2num(l)
}
\arguments{
\item{l}{A vector of lowercase letters and the character `.}
}
\value{
A vector of numbers from 1 to 27.
}
\description{
Converts a lowercase letter to its corresponding position in the alphabet
plus one. (e.g. a -> 2, z -> 27). Converts "`" to 1.
}
\examples{
letter2num(c("h","e","l","l","o"))
}
