#' Say hello
#'
#' @return
#' Flavor of "Hello".
#'
#' @export
#'
#' @examples
#' isay()
isay <- function() {
  message(praise::praise())
  h <- sample(c("Hello", "Hi", "Howdy", "Hey now", "What's up?"), size = 1)
  return(h)
}
