#' write_pop_total_scene
#'
#' @param pop_t
#' @param path
#' @param move_include
#'
#' @return
#' @export
#'
#' @examples
write_pop_total_scene <- function(pop_t, path, move_include = FALSE) {
  if (move_include == FALSE) {
    temp <- c(
      paste0(path, "/pop1_low.xlsx"),
      paste0(path, "/pop1_mid.xlsx"),
      paste0(path, "/pop1_high.xlsx"),
      paste0(path, "/pop2_low.xlsx"),
      paste0(path, "/pop2_mid.xlsx"),
      paste0(path, "/pop2_high.xlsx")
    )
    for (k in 1:6) {
      xlsx::write.xlsx(x = pop_t[[k]], file = temp[k], sheetName = "sheet1", row.names = FALSE)
    }
  } else {
    temp <- c(
      paste0(path, "/pop1_move_low.xlsx"),
      paste0(path, "/pop1_move_mid.xlsx"),
      paste0(path, "/pop1_move_high.xlsx"),
      paste0(path, "/pop2_move_low.xlsx"),
      paste0(path, "/pop2_move_mid.xlsx"),
      paste0(path, "/pop2_move_high.xlsx")
    )
    for (k in 1:6) {
      xlsx::write.xlsx(x = pop_t[[k]], file = temp[k], sheetName = "sheet1", row.names = FALSE)
    }
  }
}
