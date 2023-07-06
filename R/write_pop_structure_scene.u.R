#' Title
#'
#' @param pop_s
#' @param path
#' @param move_include
#'
#' @return
#' @export
#'
#' @examples
write_pop_structure_scene.u <- function(pop_s, path, move_include = FALSE) {
  if (move_include == FALSE) {
    temp <- c(
      paste0(path_out_table, "/urban_agegroup_low.xlsx"),
      paste0(path_out_table, "/urban_agegroup_mid.xlsx"),
      paste0(path_out_table, "/urban_agegroup_high.xlsx"),
      paste0(path_out_table, "/rural_agegroup_low.xlsx"),
      paste0(path_out_table, "/rural_agegroup_mid.xlsx"),
      paste0(path_out_table, "/rural_agegroup_high.xlsx")
    )
    for (k in 1:6) {
      counter <- 0
      for (i in 1:project_year) {
        xlsx::write.xlsx(
          x = pop_s[[k]][[i]], file = temp[k], sheetName = paste("sheet", counter, sep = ""),
          row.names = FALSE, append = T
        )
        counter <- counter + 1
      }
    }
  } else {
    temp <- c(
      paste0(path_out_table, "/urban_move_agegroup_low.xlsx"),
      paste0(path_out_table, "/urban_move_agegroup_mid.xlsx"),
      paste0(path_out_table, "/urban_move_agegroup_high.xlsx"),
      paste0(path_out_table, "/rural_move_agegroup_low.xlsx"),
      paste0(path_out_table, "/rural_move_agegroup_mid.xlsx"),
      paste0(path_out_table, "/rural_move_agegroup_high.xlsx")
    )
    for (k in 1:6) {
      counter <- 0
      for (i in 1:project_year) {
        xlsx::write.xlsx(
          x = pop_s[[k]][[i]], file = temp[k], sheetName = paste("sheet", counter, sep = ""),
          row.names = FALSE, append = T
        )
        counter <- counter + 1
      }
    }
  }
}
