#' Title
#'
#' @param pop_s
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples
f.pop_t <- function(pop_s, project_year, age_group) {
  pop_t <- pop_total_initial_scene(project_year)
  pop_t <- pop_total_cum_scene(pop_t, pop_s,
    project_year, age_group,
    sex = "all"
  )
  return(pop_t)
}
