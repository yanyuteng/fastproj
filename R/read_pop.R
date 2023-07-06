#' Title
#'
#' @param path
#'
#' @return a list
#' @export
#'
#' @examples
read_pop <- function(path) {
  infile_pop <- list()
  for (i in 1:2) {
    infile_pop[[i]] <- xlsx::read.xlsx(path, sheetIndex = i)
    infile_pop[[i]] <- as.data.frame(lapply(infile_pop[[i]], as.numeric))
    infile_pop[[i]][, 4:6] <- infile_pop[[i]][, 4:6] / 1000
  }
  return(infile_pop)
}
