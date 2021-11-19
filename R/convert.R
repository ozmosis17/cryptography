#' Convert letter to number
#'
#' Converts a lowercase letter to its corresponding position in the alphabet
#' (e.g. a -> 1, z -> 26). Converts " " to 27.
#'
#' @param l A vector of lowercase letters.
#'
#' @return A vector of numbers from 1 to 27.
#' @export
#'
#' @examples
#' letter2num(c("h","e","l","l","o"))
letter2num <- function(l) {
  n <- c(1:length(l))
  for (i in 1:length(l)) {
    if(l[i] == " ") {
      n[i] <- 27
    } else {
      n[i] <- match(l[i],letters)
    }
  }
  invisible(n)
}

#' Converts number to letter
#'
#' Converts a number 1-26 to its corresponding letter, and converts 27 to " "
#'
#' @param n A vector of numbers from 1 to 27
#'
#' @return A lowercase letter.
#' @export
#'
#' @examples
#' num2letter(c(1,2,3,4,27))
num2letter <- function(n) {
  l <- c(1:length(n))
  for (i in 1:length(n)) {
    if(n[i] > 27) {
      stop("Must be number from 1 to 27")
    } else if(n[i] == 27) {
      l[i] <- " "
    } else {
      l[i] <- letters[n[i]]
    }
  }
  invisible(l)
}
