#' Title
#'
#' @param pop_t
#' @param pop_s
#' @param project_year
#' @param age_group
#'
#' @return
#' @export
#'
#' @examples
pop_total_cum <- function(pop_t, pop_s,
                          project_year = 1, age_group = 101) {
  for (i in 1:project_year) {
    pop_t[i, 2] <- pop_s[[i]][1, 2] + pop_s[[i]][1, 3]
    pop_t[i, 3] <- round(sum(pop_s[[i]][, 4]) + sum(pop_s[[i]][, 5]), 0)
    pop_t[i, 1] <- round(sum(pop_s[[i]][, 2]) + sum(pop_s[[i]][, 3]), 0)

    for (j in 1:15) {
      pop_t[i, 4] <- pop_t[i, 4] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 16:60) {
      pop_t[i, 5] <- pop_t[i, 5] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 16:65) {
      pop_t[i, 6] <- pop_t[i, 6] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 61:age_group) {
      pop_t[i, 7] <- pop_t[i, 7] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 66:age_group) {
      pop_t[i, 8] <- pop_t[i, 8] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 81:age_group) {
      pop_t[i, 9] <- pop_t[i, 9] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }

    for (j in 1:5) {
      pop_t[i, 10] <- pop_t[i, 10] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 6:10) {
      pop_t[i, 11] <- pop_t[i, 11] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 11:15) {
      pop_t[i, 12] <- pop_t[i, 12] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 16:20) {
      pop_t[i, 13] <- pop_t[i, 13] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 21:25) {
      pop_t[i, 14] <- pop_t[i, 14] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 26:30) {
      pop_t[i, 15] <- pop_t[i, 15] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 31:35) {
      pop_t[i, 16] <- pop_t[i, 16] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 36:40) {
      pop_t[i, 17] <- pop_t[i, 17] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 41:45) {
      pop_t[i, 18] <- pop_t[i, 18] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 46:50) {
      pop_t[i, 19] <- pop_t[i, 19] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 51:55) {
      pop_t[i, 20] <- pop_t[i, 20] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 56:60) {
      pop_t[i, 21] <- pop_t[i, 21] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 61:65) {
      pop_t[i, 22] <- pop_t[i, 22] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 66:70) {
      pop_t[i, 23] <- pop_t[i, 23] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 71:75) {
      pop_t[i, 24] <- pop_t[i, 24] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 76:80) {
      pop_t[i, 25] <- pop_t[i, 25] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 81:85) {
      pop_t[i, 26] <- pop_t[i, 26] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 86:90) {
      pop_t[i, 27] <- pop_t[i, 27] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 91:95) {
      pop_t[i, 28] <- pop_t[i, 28] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 96:100) {
      pop_t[i, 29] <- pop_t[i, 29] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
    for (j in 101:101) {
      pop_t[i, 30] <- pop_t[i, 30] + pop_s[[i]][j, 2] + pop_s[[i]][j, 3]
    }
  }
  pop_t$year <- c(1:project_year) - 1
  pop_t <- pop_t[, c(31, 1:30)]
  return(pop_t)
}
