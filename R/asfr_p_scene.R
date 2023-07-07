#' Obtain ASFR parameter in Different Scenarios
#'
#' @param infile
#' @param inparm_scene
#'
#' @return
#' @export
#'
#' @examples asfr = asfr_p_scene(infile_pop, inparm_scene)
asfr_p_scene <- function(infile, inparm_scene) {
  temp <- list(list(), list(), list(), list(), list(), list())

  tfr_p <- rep(0, 101)
  tfr <- list()

  for (i in 1:3) {
    age_group <- length(infile[[1]][, 1])
    project_year <- length(inparm_scene[[1]][, 1])
    age_asfr <- infile[[1]][, 6]
    tfr_base <- sum(age_asfr)
    tfr[[i]] <- inparm_scene[[i]][, 4]
    for (j in 1:project_year) {
      for (k in 1:age_group) {
        tfr_p[k] <- tfr[[i]][j] * age_asfr[k] / tfr_base
      }
      temp[[i]][[j]] <- tfr_p
    }
  }

  for (i in 4:6) {
    age_group <- length(infile[[2]][, 1])
    project_year <- length(inparm_scene[[2]][, 1])
    age_asfr <- infile[[2]][, 6]
    tfr_base <- sum(age_asfr)
    tfr[[i]] <- inparm_scene[[i]][, 4]
    for (j in 1:project_year) {
      for (k in 1:age_group) {
        tfr_p[k] <- tfr[[i]][j] * age_asfr[k] / tfr_base
      }
      temp[[i]][[j]] <- tfr_p
    }
  }

  return(temp)
}
