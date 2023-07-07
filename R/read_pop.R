#' Obtain Baseline Population from Excel
#'
#' @param path
#'
#' @return a list
#' @export
#'
#' @examples infile_pop = read_pop('~/desktop/Pop_Projection/1_Input/Data_Population.xlsx')
read_pop <- function(path) {
  infile_pop <- list()
  for (i in 1:2) {
    infile_pop[[i]] <- xlsx::read.xlsx(path, sheetIndex = i)
    infile_pop[[i]] <- as.data.frame(lapply(infile_pop[[i]], as.numeric))
    infile_pop[[i]][, 4:6] <- infile_pop[[i]][, 4:6] / 1000
  }
  return(infile_pop)
}
