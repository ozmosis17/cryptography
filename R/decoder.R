#' Text Decoder
#'
#' Decodes a text using the Metropolis algorithm.
#'
#' @param code An encoded .txt file. Make sure all letters within are lowercase.
#' Coded text should have the ` character rather than spaces.
#' @param iter Number of iterations to run the algorithm for.
#' @param M The probability matrix of likely two-letter combinations. Defaults
#' to a probability matrix using 2-27 to represent letters and 1 to represent
#' the ` character.
#'
#' @return Decoded message in string form.
#' @export
#'
#' @examples
#' decoder("data/encodedtext1.txt", iter = 5000)
decoder <- function(code,iter = 10000, M = "data/letterprob.mat") {
  codetxt <- read.delim(code, header = FALSE, as.is = TRUE)$V1 %>%
    strsplit(split = "") %>%
    unlist()

  codenum <- letter2num(codetxt)

  letterprob <- R.matlab::readMat(M)[[1]]

  key <- sample(27)

  for (i in 1:iter) {
    rand1 <- ceiling(runif(1)*27)
    rand2 <- ceiling(runif(1)*27)
    while (rand1 == rand2) {
      rand2 <- ceiling(runif(1)*27)
    }

    # swap two random values in key to create keymaybe
    keymaybe <- replace(key, c(rand1, rand2), c(key[rand2], key[rand1]))

    diff <- loglike(codenum, keymaybe, letterprob) - loglike(codenum, key, letterprob)
    if (diff > 0) {
      key <- keymaybe
    } else if (runif(1) < exp(diff)) {
      key <- keymaybe
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
