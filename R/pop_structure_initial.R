#' Title
#'
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples
pop_structure_initial <- function(project_year = 1, age_group = 101) {
  pop_temp <- list()

  temp <- data.frame(matrix(0, nrow = age_group, ncol = 5))
  colnames(temp) <- c("age", "male", "female", "male_death", "female_death")
  temp[, 1] <- c(0:100)
  for (i in 1:project_year) {
    pop_temp[[i]] <- temp
  }
  return(pop_temp)
}
