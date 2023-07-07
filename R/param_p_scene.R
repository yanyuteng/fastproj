#' Obtain Parameters in Different Scenarios
#'
#' @param inparm_scene
#' @param type
#'
#' @return
#' @export
#'
#' @examples move_net = param_p_scene(inparm_scene,'move')
#' srb = param_p_scene(inparm_scene,'srb')
#' project_year = length(inparm_scene[[1]][,1])
#' age_group = length(infile_pop[[1]][,1])
param_p_scene <- function(inparm_scene, type) {
  if (type == "srb") {
    temp <- list()
    for (i in 1:6) {
      temp[[i]] <- inparm_scene[[i]][, 5]
    }
    return(temp)
  }

  if (type == "move") {
    temp <- list()
    for (i in 1:6) {
      temp[[i]] <- inparm_scene[[i]][, 6]
    }
    return(temp)
  }

  if (type == "urban") {
    temp <- list()
    for (i in 1:6) {
      temp[[i]] <- inparm_scene[[i]][, 7]
    }
    return(temp)
  }
}
