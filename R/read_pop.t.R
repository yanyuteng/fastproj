#' Title
#'
#' @param path_out_table
#'
#' @return
#' @export
#'
#' @examples
read_pop.t <- function(path_out_table) {
  path <- c(
    "/pop1_move_low.xlsx",
    "/pop1_move_mid.xlsx",
    "/pop1_move_high.xlsx",
    "/pop2_move_low.xlsx",
    "/pop2_move_mid.xlsx",
    "/pop2_move_high.xlsx"
  )
  path <- paste0(path_out_table, path)

  project_p1 <- list()
  for (i in 1:3) {
    project_p1[[i]] <- xlsx::read.xlsx(path[i], sheetIndex = 1)
    project_p1[[i]] <- as.data.frame(lapply(project_p1[[i]], as.numeric))
  }

  project_p2 <- list()
  for (i in 1:3) {
    project_p2[[i]] <- xlsx::read.xlsx(path[i + 3], sheetIndex = 1)
    project_p2[[i]] <- as.data.frame(lapply(project_p2[[i]], as.numeric))
  }

  project_p <- list(project_p1, project_p2)
  return(project_p)
}
