#' Obtain Projection Parameters from Excel
#'
#' @param path
#'
#' @return a list
#' @export
#'
#' @examples inparm_scene = read_param('~/desktop/Pop_Projection/1_Input/Data_Parameter.xlsx')
read_param <- function(path) {
  inparm_scene <- list()
  for (i in 1:6) {
    inparm_scene[[i]] <- xlsx::read.xlsx(path, sheetIndex = i)
  }
  return(inparm_scene)
}
