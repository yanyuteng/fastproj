#' Fast Converting Population Structure into Summary Data
#' @description Aggregated data, which displays as 5 age group data
#' @param pop_s
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples pop_t = f.pop_t(pop_s, project_year, age_group)
f.pop_t <- function(pop_s, project_year, age_group) {
  pop_t <- pop_total_initial_scene(project_year)
  pop_t <- pop_total_cum_scene(pop_t, pop_s,
    project_year, age_group,
    sex = "all"
  )
  return(pop_t)
}
