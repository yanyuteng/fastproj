# 移民分年龄别分布按当年人口结构
#' Title
#'
#' @param pop_s
#' @param move_net
#' @param age_group
#' @param project_year
#'
#' @return
#' @export
#'
#' @examples
pop_structure_immigration <- function(pop_s, move_net, age_group = 101, project_year = 1) {
  move_s <- list()

  temp <- c()
  temp.s <- data.frame(matrix(0, nrow = age_group, ncol = 5))
  temp.s[, 1] <- c(0:100)
  colnames(temp.s) <- c("age", "male", "female", "male_r", "female_r")

  for (i in 1:project_year) {
    temp[i] <- cumsum(pop_s[[i]][, 2])[age_group] + cumsum(pop_s[[i]][, 3])[age_group]
    for (j in 1:age_group) {
      temp.s[j, 4] <- pop_s[[i]][j, 2] / temp[i]
      temp.s[j, 5] <- pop_s[[i]][j, 3] / temp[i]
      temp.s[j, 2] <- round(move_net[i] * temp.s[j, 4], 0)
      temp.s[j, 3] <- round(move_net[i] * temp.s[j, 5], 0)
    }
    move_s[[i]] <- temp.s
  }
  return(move_s)
}
