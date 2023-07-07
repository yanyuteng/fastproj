#' Summary Projection Population
#'
#' @param pop_s
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
pop_structure_cum <- function(pop_s,
                              project_year = 2, age_group = 101,
                              Lx_male, Lx_female, asfr, srb) {
  for (i in 2:project_year) {
    # 1 Death Population
    for (j in 1:(age_group - 1)) {
      pop_s[[i]][j + 1, 4] <- round(pop_s[[i - 1]][j, 2] * (1 - Lx_male[[i]][j + 1] / Lx_male[[i]][j]), 0)
      pop_s[[i]][j + 1, 5] <- round(pop_s[[i - 1]][j, 3] * (1 - Lx_female[[i]][j + 1] / Lx_female[[i]][j]), 0)
    }
    # Death at Infant
    pop_s[[i]][1, 4] <- round(pop_s[[i - 1]][1, 2] * (1 - Lx_male[[i]][1]), 0)
    pop_s[[i]][1, 5] <- round(pop_s[[i - 1]][1, 3] * (1 - Lx_female[[i]][1]), 0)

    # 2 人口转移矩阵
    for (j in 1:(age_group - 1)) {
      # age_group以上年龄不再计算
      pop_s[[i]][j + 1, 2] <- round(pop_s[[i - 1]][j, 2] * (Lx_male[[i]][j + 1] / Lx_male[[i]][j]), 0)
      pop_s[[i]][j + 1, 3] <- round(pop_s[[i - 1]][j, 3] * (Lx_female[[i]][j + 1] / Lx_female[[i]][j]), 0)
    }

    # 3 Birth Population
    temp <- 0
    temp_list <- list()
    for (j in 15:49) {
      temp <- temp + asfr[[i]][j] * 0.5 * (pop_s[[i - 1]][j, 3] +
        pop_s[[i - 1]][j - 1, 3] * Lx_female[[i]][j] / Lx_female[[i]][j - 1])
      temp_list[i] <- round(temp, 0)
      pop_s[[i]][1, 2] <- round(temp * (srb[i] / (100 + srb[i])) * Lx_male[[i]][1], 0)
      pop_s[[i]][1, 3] <- round(temp * (1 - srb[i] / (100 + srb[i])) * Lx_female[[i]][1], 0)
    }
  }
  return(pop_s)
}
