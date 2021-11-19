#' Text Decoder
#'
#' Decodes a text using the Metropolis algorithm.
#'
#' @importFrom dplyr '%>%'
#'
#' @param code An encoded .txt file. Make sure all letters within are lowercase.
#' Decoded test will preserve periods (.).
#' @param iter Number of iterations to run the algorithm for.
#' @param M The probability matrix of likely two-letter combinations.
#'
#' @return Decoded message in string form.
#' @export
#'
#' @examples
decoder <- function(code,iter,M) {
  codetxt <- read.delim(code, header = FALSE, as.is = TRUE)$V1 %>%
    strsplit(split = "") %>%
    unlist()

  codenum <- c(1:length(codetxt))
  for (i in 1:length(codetxt)) {
    codenum[i] <- lettertonum(codetxt[i])
  }

  letterprob <- R.matlab::readMat(M) %>%
    unlist()

  key <- sample(27)

  for (i in 1:iter) {
    rand1 <- ceiling(runif(1)*27)
    rand2 <- ceiling(runif(1)*27)
    while (rand1 == rand2) {
      rand2 <- ceiling(runif(1)*27)
    }

    # swap two random values in key to create keymaybe
    keymaybe <- replace(key, c(rand1, rand2), c(key[rand1], key[rand2]))

    diff <- loglike(codenum, keymaybe, M) - loglike(codenum, key, M)
    if (diff > 0) {
      key <- keymaybe
    } else if (runif(1) < exp(diff)) {
      key <- keymaybe
    }

    answer <- key[codenum] %>%
      numtoletter()
  }
}

#' Title
#'
#' @param textnum
#' @param y
#' @param M
#'
#' @return
#' @export
#'
#' @examples
loglike <- function(textnum, y, M) {
  val = 0
  k = length(textnum)
  for (i in 1:(k-1)) {
    val <- val + log(M[y[textnum[i]],y[textnum[i+1]]])
  }
  invisible(val)
}
