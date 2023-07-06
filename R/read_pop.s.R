#' Title
#'
#' @param path_out_table
#' @param project_year
#'
#' @return
#' @export
#'
#' @examples
read_pop.s <- function(path_out_table, project_year) {
  path <- c(
    "/pop1_move_agegroup_low.xlsx",
    "/pop1_move_agegroup_mid.xlsx",
    "/pop1_move_agegroup_high.xlsx",
    "/pop2_move_agegroup_low.xlsx",
    "/pop2_move_agegroup_mid.xlsx",
    "/pop2_move_agegroup_high.xlsx"
  )
  path <- paste0(path_out_table, path)

  project_p1_s <- list(list(), list(), list())
  for (j in 1:3) {
    for (i in c(1:project_year)) {
      project_p1_s[[j]][[i]] <- xlsx::read.xlsx(path[j], sheetIndex = i)
      project_p1_s[[j]][[i]] <- as.data.frame(lapply(project_p1_s[[j]][[i]], as.numeric))
    }
  }

  project_p2_s <- list(list(), list(), list())
  for (j in 1:3) {
    for (i in c(1:project_year)) {
      project_p2_s[[j]][[i]] <- xlsx::read.xlsx(path[3 + j], sheetIndex = i)
      project_p2_s[[j]][[i]] <- as.data.frame(lapply(project_p2_s[[j]][[i]], as.numeric))
    }
  }

  project_p_s <- list(project_p1_s, project_p2_s)
  return(project_p_s)
}
