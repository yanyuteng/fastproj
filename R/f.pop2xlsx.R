#' Title
#'
#' @param path_out_table
#' @param pop_s
#' @param pop_move_s
#' @param pop_t
#' @param pop_move_t
#'
#' @return
#' @export
#'
#' @examples
f.pop2xlsx <- function(path_out_table,
                       pop_s, pop_move_s, pop_t, pop_move_t) {
  write_pop_structure_scene(pop_s, path_out_table, move_include = FALSE)
  write_pop_structure_scene(pop_move_s, path_out_table, move_include = TRUE)
  write_pop_total_scene(pop_t, path_out_table, move_include = FALSE)
  write_pop_total_scene(pop_move_t, path_out_table, move_include = TRUE)
}
