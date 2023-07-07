#' Fast Obtain Population Structure include Immigration
#'
#' @param pop_s
#' @param project_year
#' @param age_group
#' @param Lx_male
#' @param Lx_female
#' @param asfr
#' @param srb
#' @param move_net
#'
#' @return
#' @export
#'
#' @examples pop_move_s = f.pop_move_s(pop_s, project_year, age_group, Lx_male, Lx_female, asfr, srb, move_net)
f.pop_move_s <- function(pop_s, project_year, age_group, Lx_male, Lx_female, asfr, srb, move_net) {
  # pop_structure_immigration_scene()基于封闭人口的比重分配，需要先计算封闭人口
  move_s <- pop_structure_immigration_scene(pop_s, move_net, project_year, age_group)
  pop_move_s <- pop_structure_immigration_cum_scene(
    pop_s, move_s,
    project_year, age_group,
    Lx_male, Lx_female, asfr, srb
  )
  return(pop_move_s)
}
