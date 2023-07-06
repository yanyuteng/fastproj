#' Title
#'
#' @param pop_s
#' @param move_s
#' @param urban
#' @param project_year
#' @param age_group
#' @param Lx_male
#' @param Lx_female
#' @param asfr
#' @param srb
#'
#' @return
#' @export
#'
#' @examples
pop_structure_immigration_cum_scene.u <- function(pop_s, move_s, urban,
                                                  project_year = 2, age_group = 101,
                                                  Lx_male, Lx_female, asfr, srb) {
  for (k in 1:6) {
    for (i in 2:project_year) {
      # 1 Death Population
      for (j in 1:(age_group - 1)) {
        pop_s[[k]][[i]][j + 1, 4] <- round((pop_s[[k]][[i - 1]][j, 2] + move_s[[k]][[i]][j, 2]) * urban[[k]][i] * (1 - Lx_male[[k]][[i]][j + 1] / Lx_male[[k]][[i]][j]), 0)
        pop_s[[k]][[i]][j + 1, 5] <- round((pop_s[[k]][[i - 1]][j, 3] + move_s[[k]][[i]][j, 3]) * urban[[k]][i] * (1 - Lx_female[[k]][[i]][j + 1] / Lx_female[[k]][[i]][j]), 0)
      }
      # Death at Infant
      pop_s[[k]][[i]][1, 4] <- round((pop_s[[k]][[i - 1]][1, 2] + move_s[[k]][[i]][1, 2]) * urban[[k]][i] * (1 - Lx_male[[k]][[i]][1]), 0)
      pop_s[[k]][[i]][1, 5] <- round((pop_s[[k]][[i - 1]][1, 3] + move_s[[k]][[i]][1, 3]) * urban[[k]][i] * (1 - Lx_female[[k]][[i]][1]), 0)

      # 2 人口转移矩阵
      for (j in 1:(age_group - 1)) {
        # age_group以上年龄不再计算
        pop_s[[k]][[i]][j + 1, 2] <- round((pop_s[[k]][[i - 1]][j, 2] + move_s[[k]][[i]][j, 2]) * urban[[k]][i] * (Lx_male[[k]][[i]][j + 1] / Lx_male[[k]][[i]][j]), 0)
        pop_s[[k]][[i]][j + 1, 3] <- round((pop_s[[k]][[i - 1]][j, 3] + move_s[[k]][[i]][j, 3]) * urban[[k]][i] * (Lx_female[[k]][[i]][j + 1] / Lx_female[[k]][[i]][j]), 0)
      }

      # 3 Birth Population
      temp <- 0
      for (j in 15:49) {
        temp <- temp + urban[[k]][i] * asfr[[k]][[i]][j] * 0.5 * (pop_s[[k]][[i - 1]][j, 3] + move_s[[k]][[i]][j, 3] +
          (pop_s[[k]][[i - 1]][j - 1, 3] + move_s[[k]][[i]][j - 1, 3]) * Lx_female[[k]][[i]][j] / Lx_female[[k]][[i]][j - 1])
        pop_s[[k]][[i]][1, 2] <- round(temp * (srb[[k]][i] / (100 + srb[[k]][i])) * Lx_male[[k]][[i]][1], 0)
        pop_s[[k]][[i]][1, 3] <- round(temp * (1 - srb[[k]][i] / (100 + srb[[k]][i])) * Lx_female[[k]][[i]][1], 0)
      }
    }
  }
  return(pop_s)
}
