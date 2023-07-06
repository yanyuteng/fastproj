#' Title
#'
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples
pop_structure_initial_scene <- function(project_year, age_group) {
  pop_temp <- list(list(), list(), list(), list(), list(), list())

  temp <- data.frame(matrix(0, nrow = age_group, ncol = 5))
  colnames(temp) <- c("age", "male", "female", "male_death", "female_death")
  temp[, 1] <- c(0:100)
  for (i in 1:6) {
    for (j in 1:project_year) {
      pop_temp[[i]][[j]] <- temp
    }
  }
  return(pop_temp)
}
