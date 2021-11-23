#' Encode some text
#'
#' @param text Text to be encoded
#'
#' @return String of encoded text
#' @export
#'
#' @examples
#' encode("this text will be encoded")
encode <- function(text) {

  textnum <- make_codable(text) %>%
    letter2num()

  key <- sample(27)

  codedtext <- key[textnum] %>%
    num2letter() %>%
    paste0(collapse = '')

  return(codedtext)
}
