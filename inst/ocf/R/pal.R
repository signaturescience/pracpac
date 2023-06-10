#' Get palette
#'
#' @param palette Character specifying wich palette to use; default is `NULL` to randomly pick a palette
#' @return Color palette
#' @export
#'
get_pal <- function(palette = NULL) {
  if(is.null(palette)) {
    n_pals <- length(wesanderson::wes_palettes)
    wesanderson::wes_palettes[sample(x=1:n_pals,size=1)]
  } else {
    wesanderson::wes_palettes[palette]
  }
}
