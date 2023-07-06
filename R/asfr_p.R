#' Title
#'
#' @param project_year
#' @param age_group
#' @param age_asfr
#' @param tfr
#'
#' @return
#' @export
#'
#' @examples
asfr_p <- function(project_year = 1, age_group = 101,
                   age_asfr, tfr) {
  tfr_p <- list()
  temp <- rep(0, 101)
  tfr_base <- sum(age_asfr)
  for (i in 1:project_year) {
    for (j in 1:age_group) {
      temp[j] <- tfr[i] * age_asfr[j] / tfr_base
    }
    tfr_p[[i]] <- temp
  }
  return(tfr_p)
}
