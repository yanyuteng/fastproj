#' Title
#'
#' @param pop_t
#' @param path
#' @param move_include
#'
#' @return
#' @export
#'
#' @examples
write_pop_total_scene.u <- function(pop_t, path, move_include = FALSE) {
  if (move_include == FALSE) {
    temp <- c(
      paste0(path, "/urban_low.xlsx"),
      paste0(path, "/urban_mid.xlsx"),
      paste0(path, "/urban_high.xlsx"),
      paste0(path, "/rural_low.xlsx"),
      paste0(path, "/rural_mid.xlsx"),
      paste0(path, "/rural_high.xlsx")
    )
    for (k in 1:6) {
      xlsx::write.xlsx(x = pop_t[[k]], file = temp[k], sheetName = "sheet1", row.names = FALSE)
    }
  } else {
    temp <- c(
      paste0(path, "/urban_move_low.xlsx"),
      paste0(path, "/urban_move_mid.xlsx"),
      paste0(path, "/urban_move_high.xlsx"),
      paste0(path, "/rural_move_low.xlsx"),
      paste0(path, "/rural_move_mid.xlsx"),
      paste0(path, "/rural_move_high.xlsx")
    )
    for (k in 1:6) {
      xlsx::write.xlsx(x = pop_t[[k]], file = temp[k], sheetName = "sheet1", row.names = FALSE)
    }
  }
}
