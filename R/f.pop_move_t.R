#' Title
#'
#' @param pop_move_s
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples
f.pop_move_t <- function(pop_move_s, project_year, age_group) {
  pop_move_t <- pop_total_initial_scene(project_year)
  pop_move_t <- pop_total_cum_scene(pop_move_t, pop_move_s,
    project_year, age_group,
    sex = "all"
  )
  return(pop_move_t)
}
