#' Output Population Structure in Different Scenarios
#' @description Stable Population
#' @param pop_s
#' @param path
#' @param move_include
#'
#' @return
#' @export
#'
#' @examples
write_pop_structure_scene <- function(pop_s, path, move_include = FALSE) {
  if (move_include == FALSE) {
    temp <- c(
      paste0(path_out_table, "/pop1_agegroup_low.xlsx"),
      paste0(path_out_table, "/pop1_agegroup_mid.xlsx"),
      paste0(path_out_table, "/pop1_agegroup_high.xlsx"),
      paste0(path_out_table, "/pop2_agegroup_low.xlsx"),
      paste0(path_out_table, "/pop2_agegroup_mid.xlsx"),
      paste0(path_out_table, "/pop2_agegroup_high.xlsx")
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
      paste0(path_out_table, "/pop1_move_agegroup_low.xlsx"),
      paste0(path_out_table, "/pop1_move_agegroup_mid.xlsx"),
      paste0(path_out_table, "/pop1_move_agegroup_high.xlsx"),
      paste0(path_out_table, "/pop2_move_agegroup_low.xlsx"),
      paste0(path_out_table, "/pop2_move_agegroup_mid.xlsx"),
      paste0(path_out_table, "/pop2_move_agegroup_high.xlsx")
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
