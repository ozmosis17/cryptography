#' Convert letter to number
#'
#' Converts a lowercase letter to its corresponding position in the alphabet
#' plus one. (e.g. a -> 2, z -> 27). Converts "`" to 1.
#'
#' @param l A vector of lowercase letters and the character `.
#'
#' @return A vector of numbers from 1 to 27.
#' @export
#'
#' @examples
#' letter2num(c("h","e","l","l","o"))
letter2num <- function(l) {
  n <- c(1:length(l))
  for (i in 1:length(l)) {
    if(l[i] == "`") {
      n[i] <- 1
    } else {
      n[i] <- match(l[i],letters) + 1
    }
  }
  invisible(n)
}

#' Converts number to letter
#'
#' Converts a number 2-27 to its corresponding letter, and converts 1 to "`"
#'
#' @param n A vector of numbers from 1 to 27
#'
#' @return A lowercase letter or `.
#' @export
#'
#' @examples
#' num2letter(c(1,2,3,4,27))
num2letter <- function(n) {
  l <- c(1:length(n))
  for (i in 1:length(n)) {
    if(n[i] > 27) {
      stop("Must be number from 1 to 27")
    } else if(n[i] == 1) {
      l[i] <- "`"
    } else {
      l[i] <- letters[n[i] - 1]
    }
  }
  invisible(l)
}

#' Converts text with proper punctuation into text that is codable by converting
#' uppercase to lowercase and removing punctuation
#'
#' @param text Text that you're trying to make codable
#'
#' @return text that is now codable
#' @export
#'
#' @examples
#' make_codable("Hello. My name is Oliver; what's your name?")
make_codable <- function(text) {
  codetext <- gsub(" ", "`", text) %>%
    strsplit(split = "") %>%
    unlist()

  codetext <- codetext[!codetext %in% c(".", ",", ";", ":", "'", "-", "?", "!")]

  ifelse(codetext == toupper(codetext), tolower(codetext), codetext)
}
