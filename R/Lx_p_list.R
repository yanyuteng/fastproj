#'
#'
#' @param ex_p
#' @param lx_p
#' @param axn
#' @param age_group
#' @param project_year
#'
#' @return
#' @export
#'
#' @examples
Lx_p_list <- function(ex_p, lx_p, axn, age_group, project_year = 1) {
  temp <- list()
  for (i in 1:project_year) {
    temp[[i]] <- Lx_p(ex_p[i], lx_p, axn, age_group)
  }
  return(temp)
}
