#' Title
#'
#' @param inparm_scene
#' @param type
#'
#' @return
#' @export
#'
#' @examples
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
