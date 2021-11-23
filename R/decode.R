#' Text Decoder
#'
#' Decodes a text using the Metropolis algorithm.
#'
#' @importFrom dplyr "%>%"
#'
#' @param codetxt An encoded string of text
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
#' decode("pkfiurxpkumuvghkgbsluhdimdc", iter = 10000, key =
#' c(12,13,8,26,19,11,4,10,5,6,18,15,21,2,24,3,16,27,22,9,7,1,14,23,17,25,20))
decode <- function(codetxt, iter = 7000, M = NULL, key = NULL) {

  # if user does not provide a probability matrix, load the default one
  # (is this necessary??)
  if (is.null(M)){
    utils::data("letterprob")
  } else {
    letterprob <- M
  }

  # convert input text into vector of individual characters
  codenum <- strsplit(codetxt, split = "") %>%
    unlist() %>%
    letter2num()

  # if user doesn't provide a key and wants algorithm to figure it out
  if(is.null(key)) {

    key <- sample(27) # generate random key

    for (i in 1:iter) {

      # swap two random values in key to create new (possibly better) key
      rand1 <- ceiling(stats::runif(1)*27)
      rand2 <- ceiling(stats::runif(1)*27)
      while (rand1 == rand2) {
        rand2 <- ceiling(stats::runif(1)*27)
      }
      newkey <- replace(key, c(rand1, rand2), c(key[rand2], key[rand1]))

      # calculate which key is better
      diff <- loglike(codenum, newkey, letterprob) - loglike(codenum, key, letterprob)
      if (diff > 0) {
        key <- newkey # overwrite old key if new key is better
      } else if (stats::runif(1) < exp(diff)) {
        key <- newkey
      }
    }
  }

  # convert answer integer vector back into single character string
  answer <- key[codenum] %>%
    num2letter() %>%
    paste0(collapse = '')

  # replace ` with spaces
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

