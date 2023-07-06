#' Title
#'
#' @param inparm_lt
#' @param inparm_scene
#' @param sex
#'
#' @return
#' @export
#'
#' @examples
Lx_p_list_scene <- function(inparm_lt, inparm_scene, sex) {
  temp <- list(list(), list(), list(), list(), list(), list())
  ex_p <- list()
  lx_p <- list()
  axn <- list()
  age_group <- length(inparm_lt[[1]][, 1])
  project_year <- length(inparm_scene[[1]][, 1])

  if (sex == "male") {
    for (i in 1:6) {
      ex_p[[i]] <- inparm_scene[[i]][, 2]
      lx_p[[i]] <- inparm_lt[[1]][, 5]
      axn[[i]] <- inparm_lt[[1]][, 2]
      for (j in 1:project_year) {
        temp[[i]][[j]] <- Lx_p(ex_p[[i]][j], lx_p[[i]], axn[[i]], age_group)
      }
    }
    return(temp)
  }
  if (sex == "female") {
    for (i in 1:6) {
      ex_p[[i]] <- inparm_scene[[i]][, 3]
      lx_p[[i]] <- inparm_lt[[2]][, 5]
      axn[[i]] <- inparm_lt[[2]][, 2]
      for (j in 1:project_year) {
        temp[[i]][[j]] <- Lx_p(ex_p[[i]][j], lx_p[[i]], axn[[i]], age_group)
      }
    }
    return(temp)
  }
}
