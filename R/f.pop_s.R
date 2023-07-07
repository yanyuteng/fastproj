#' Fast Obtain Population Structure
#' @description Stable Population exclude immigration
#' @param infile_pop
#' @param project_year
#' @param age_group
#' @param Lx_male
#' @param Lx_female
#' @param asfr
#' @param srb
#'
#' @return
#' @export
#'
#' @examples pop_s = f.pop_s(infile_pop, project_year, age_group, Lx_male, Lx_female, asfr, srb)
f.pop_s <- function(infile_pop, project_year, age_group, Lx_male, Lx_female, asfr, srb) {
  # 1. Project Baseline
  pop_s <- pop_structure_initial_scene(project_year, age_group)
  pop_s <- pop_structure_baseline_scene(pop_s, infile_pop)
  # 2. Project (Staged Population)
  pop_s <- pop_structure_cum_scene(
    pop_s,
    project_year, age_group,
    Lx_male, Lx_female, asfr, srb
  )
  return(pop_s)
}
