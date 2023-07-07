#' Fast Projection and Output to Excel
#'
#' @param infile_pop
#' @param inparm_scene
#' @param inparm_lt
#' @param path_out_table
#'
#' @return
#' @export
#'
#' @examples path_out_table = '~/Desktop/Pop_Projection/2_Output_Table'
#' f.projection(infile_pop, inparm_scene, inparm_lt, path_out_table)
f.projection <- function(infile_pop, inparm_scene, inparm_lt, path_out_table) {
  if (dir.exists(path_out_table) == TRUE) {
    unlink(path_out_table, recursive = TRUE)
    dir.create(path_out_table)
  }
  if (dir.exists(path_out_table) == FALSE) {
    dir.create(path_out_table)
  }
  # 1. Inparm Input
  move_net <- param_p_scene(inparm_scene, "move")
  srb <- param_p_scene(inparm_scene, "srb")
  project_year <- length(inparm_scene[[1]][, 1])
  age_group <- length(infile_pop[[1]][, 1])

  # 2. Inparm Project: Lx and ASFR
  Lx_male <- Lx_p_list_scene(inparm_lt, inparm_scene, "male")
  Lx_female <- Lx_p_list_scene(inparm_lt, inparm_scene, "female")
  asfr <- asfr_p_scene(infile_pop, inparm_scene)

  # 3. Pop Project
  # staged population
  pop_s <- f.pop_s(infile_pop, project_year, age_group, Lx_male, Lx_female, asfr, srb)
  pop_t <- f.pop_t(pop_s, project_year, age_group)
  # immigration population
  pop_move_s <- f.pop_move_s(pop_s, project_year, age_group, Lx_male, Lx_female, asfr, srb, move_net)
  pop_move_t <- f.pop_move_t(pop_move_s, project_year, age_group)

  # 4. Output
  f.pop2xlsx(path_out_table, pop_s, pop_move_s, pop_t, pop_move_t)
}
