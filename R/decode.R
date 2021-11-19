#' Text Decoder
#'
#' Decodes a text using the Metropolis algorithm.
#'
#' @importFrom dplyr "%>%"
#'
#' @param code An encoded .txt file. Make sure all letters within are lowercase.
#'   Coded text should have the ` character rather than spaces.
#' @param iter Number of iterations to run the algorithm for.
#' @param M A 27x27 probability matrix of likely two-letter combinations.
#'   Defaults to a probability matrix using 2-27 to represent letters and 1 to
#'   represent the ` character.
#' @param key If the user already has the key to decode the encoded message,
#'   they can insert it here as a 1x27 vector of numbers where the index of the
#'   key is the number representing the coded letter and the value in the key is
#'   the number representing the decoded letter. Remember that a value of 1
#'   represents a ` or a space, and that 2-27 represent the letters in
#'   alphabetical order. For example, if "c"s in the code represent spaces, the
#'   4th entry of the key should be a 1.
#' @return Decoded message in string form.
#' @export
#'
#' @examples
#' decode("~/Documents/encodedtext1.txt", iter = 10000)
decode <- function(code, iter = 7000, M = NULL, key = NULL) {
  codetxt <- utils::read.delim(code, header = FALSE, as.is = TRUE)$V1 %>%
    strsplit(split = "") %>%
    unlist()

  codenum <- letter2num(codetxt)

  # if user doesn't provide a key and wants algorithm to figure it out
  if(is.null(key)) {

    key <- sample(27)

    for (i in 1:iter) {
      rand1 <- ceiling(stats::runif(1)*27)
      rand2 <- ceiling(stats::runif(1)*27)
      while (rand1 == rand2) {
        rand2 <- ceiling(stats::runif(1)*27)
      }

      # swap two random values in key to create keymaybe
      keymaybe <- replace(key, c(rand1, rand2), c(key[rand2], key[rand1]))

      if (is.null(M)){
        utils::data("letterprob")
      } else {
        letterprob <- M
      }

      diff <- loglike(codenum, keymaybe, letterprob) - loglike(codenum, key, letterprob)
      if (diff > 0) {
        key <- keymaybe
      } else if (stats::runif(1) < exp(diff)) {
        key <- keymaybe
      }
    }
  }

  answer <- key[codenum] %>%
    num2letter() %>%
    paste0(collapse = '')

  answer <- gsub("`", " ", answer)

  return(answer)
}

#' Calculate the log-likelihood of this key based on probability matrix
#'
#' @param textnum The encoded text, converted to a vector of chars
#' @param y The key that you're testing
#' @param M The probability matrix of likely two letter combinations
#'
#' @return The log-likelihood of this key
#'
loglike <- function(textnum, y, M) {
  val = 0
  k = length(textnum)
  for (i in 1:(k-1)) {
    val <- val + log(M[y[textnum[i]],y[textnum[i+1]]])
  }
  invisible(val)
}

