#' Title
#'
#' @param pop_s
#' @param move_net
#' @param urban
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples
pop_structure_immigration_scene.u <- function(pop_s, move_net, urban, project_year = 1, age_group = 101) {
  move_s <- list(list(), list(), list(), list(), list(), list())
  temp <- c()
  temp.s <- data.frame(matrix(0, nrow = age_group, ncol = 5))
  temp.s[, 1] <- c(0:100)
  colnames(temp.s) <- c("age", "male", "female", "male_r", "female_r")

  for (k in 1:6) {
    for (i in 1:project_year) {
      temp[i] <- sum(pop_s[[k]][[i]][, 2]) + sum(pop_s[[k]][[i]][, 3])
      for (j in 1:age_group) {
        temp.s[j, 4] <- (pop_s[[k]][[i]][j, 2] * urban[[k]][i]) / temp[i]
        temp.s[j, 5] <- (pop_s[[k]][[i]][j, 3] * urban[[k]][i]) / temp[i]
        temp.s[j, 2] <- round(move_net[[k]][i] * temp.s[j, 4] * urban[[k]][i], 0)
        temp.s[j, 3] <- round(move_net[[k]][i] * temp.s[j, 5] * urban[[k]][i], 0)
      }
      move_s[[k]][[i]] <- temp.s
    }
  }
  return(move_s)
}
