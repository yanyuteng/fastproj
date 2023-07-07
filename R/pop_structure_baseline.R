#' Initial Baseline Population Structure
#'
#' @param pop_s
#' @param infile
#'
#' @return
#' @export
#'
#' @examples
pop_structure_baseline <- function(pop_s, infile) {
  pop_s[[1]][, 2] <- infile[, 2]
  pop_s[[1]][, 3] <- infile[, 3]
  pop_s[[1]][, 4] <- round(infile[, 2] * infile[, 4], 0)
  pop_s[[1]][, 5] <- round(infile[, 3] * infile[, 5], 0)
  return(pop_s)
}
