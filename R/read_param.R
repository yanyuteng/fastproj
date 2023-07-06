#' Title
#'
#' @param path
#'
#' @return a list
#' @export
#'
#' @examples
read_param <- function(path) {
  inparm_scene <- list()
  for (i in 1:6) {
    inparm_scene[[i]] <- xlsx::read.xlsx(path, sheetIndex = i)
  }
  return(inparm_scene)
}
