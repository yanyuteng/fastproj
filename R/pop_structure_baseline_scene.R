#' Title
#'
#' @param pop_s
#' @param infile
#'
#' @return
#' @export
#'
#' @examples
pop_structure_baseline_scene <- function(pop_s, infile) {
  for (i in 1:3) {
    pop_s[[i]][[1]][, 2] <- infile[[1]][, 2]
    pop_s[[i]][[1]][, 3] <- infile[[1]][, 3]
    pop_s[[i]][[1]][, 4] <- round(infile[[1]][, 2] * infile[[1]][, 4], 0)
    pop_s[[i]][[1]][, 5] <- round(infile[[1]][, 3] * infile[[1]][, 5], 0)
  }
  for (i in 4:6) {
    pop_s[[i]][[1]][, 2] <- infile[[2]][, 2]
    pop_s[[i]][[1]][, 3] <- infile[[2]][, 3]
    pop_s[[i]][[1]][, 4] <- round(infile[[2]][, 2] * infile[[2]][, 4], 0)
    pop_s[[i]][[1]][, 5] <- round(infile[[2]][, 3] * infile[[2]][, 5], 0)
  }
  return(pop_s)
}
