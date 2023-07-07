#' pop_total_cum_scene
#'
#' @param pop_t
#' @param pop_s
#' @param project_year
#' @param age_group
#' @param sex
#'
#' @return
#' @export
#'
#' @examples
pop_total_cum_scene <- function(pop_t, pop_s,
                                project_year = 1, age_group = 101, sex = "all") {
  if (sex == "all") {
    for (k in 1:6) {
      for (i in 1:project_year) {
        pop_t[[k]][i, 2] <- pop_s[[k]][[i]][1, 2] + pop_s[[k]][[i]][1, 3]
        pop_t[[k]][i, 3] <- round(sum(pop_s[[k]][[i]][, 4]) + sum(pop_s[[k]][[i]][, 5]), 0)
        pop_t[[k]][i, 1] <- round(sum(pop_s[[k]][[i]][, 2]) + sum(pop_s[[k]][[i]][, 3]), 0)

        for (j in 1:15) {
          pop_t[[k]][i, 4] <- pop_t[[k]][i, 4] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 16:60) {
          pop_t[[k]][i, 5] <- pop_t[[k]][i, 5] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 16:65) {
          pop_t[[k]][i, 6] <- pop_t[[k]][i, 6] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 61:age_group) {
          pop_t[[k]][i, 7] <- pop_t[[k]][i, 7] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 66:age_group) {
          pop_t[[k]][i, 8] <- pop_t[[k]][i, 8] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 81:age_group) {
          pop_t[[k]][i, 9] <- pop_t[[k]][i, 9] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }

        for (j in 1:5) {
          pop_t[[k]][i, 10] <- pop_t[[k]][i, 10] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 6:10) {
          pop_t[[k]][i, 11] <- pop_t[[k]][i, 11] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 11:15) {
          pop_t[[k]][i, 12] <- pop_t[[k]][i, 12] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 16:20) {
          pop_t[[k]][i, 13] <- pop_t[[k]][i, 13] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 21:25) {
          pop_t[[k]][i, 14] <- pop_t[[k]][i, 14] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 26:30) {
          pop_t[[k]][i, 15] <- pop_t[[k]][i, 15] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 31:35) {
          pop_t[[k]][i, 16] <- pop_t[[k]][i, 16] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 36:40) {
          pop_t[[k]][i, 17] <- pop_t[[k]][i, 17] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 41:45) {
          pop_t[[k]][i, 18] <- pop_t[[k]][i, 18] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 46:50) {
          pop_t[[k]][i, 19] <- pop_t[[k]][i, 19] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 51:55) {
          pop_t[[k]][i, 20] <- pop_t[[k]][i, 20] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 56:60) {
          pop_t[[k]][i, 21] <- pop_t[[k]][i, 21] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 61:65) {
          pop_t[[k]][i, 22] <- pop_t[[k]][i, 22] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 66:70) {
          pop_t[[k]][i, 23] <- pop_t[[k]][i, 23] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 71:75) {
          pop_t[[k]][i, 24] <- pop_t[[k]][i, 24] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 76:80) {
          pop_t[[k]][i, 25] <- pop_t[[k]][i, 25] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 81:85) {
          pop_t[[k]][i, 26] <- pop_t[[k]][i, 26] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 86:90) {
          pop_t[[k]][i, 27] <- pop_t[[k]][i, 27] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 91:95) {
          pop_t[[k]][i, 28] <- pop_t[[k]][i, 28] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 96:100) {
          pop_t[[k]][i, 29] <- pop_t[[k]][i, 29] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 101:101) {
          pop_t[[k]][i, 30] <- pop_t[[k]][i, 30] + pop_s[[k]][[i]][j, 2] + pop_s[[k]][[i]][j, 3]
        }
      }
      pop_t[[k]]$year <- c(1:project_year) - 1
      pop_t[[k]] <- pop_t[[k]][, c(31, 1:30)]
    }
    return(pop_t)
  }

  if (sex == "male") {
    for (k in 1:6) {
      for (i in 1:project_year) {
        pop_t[[k]][i, 2] <- pop_s[[k]][[i]][1, 2]
        pop_t[[k]][i, 3] <- round(sum(pop_s[[k]][[i]][, 4]), 0)
        pop_t[[k]][i, 1] <- round(sum(pop_s[[k]][[i]][, 2]), 0)

        for (j in 1:15) {
          pop_t[[k]][i, 4] <- pop_t[[k]][i, 4] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 16:60) {
          pop_t[[k]][i, 5] <- pop_t[[k]][i, 5] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 16:65) {
          pop_t[[k]][i, 6] <- pop_t[[k]][i, 6] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 61:age_group) {
          pop_t[[k]][i, 7] <- pop_t[[k]][i, 7] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 66:age_group) {
          pop_t[[k]][i, 8] <- pop_t[[k]][i, 8] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 81:age_group) {
          pop_t[[k]][i, 9] <- pop_t[[k]][i, 9] + pop_s[[k]][[i]][j, 2]
        }

        for (j in 1:5) {
          pop_t[[k]][i, 10] <- pop_t[[k]][i, 10] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 6:10) {
          pop_t[[k]][i, 11] <- pop_t[[k]][i, 11] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 11:15) {
          pop_t[[k]][i, 12] <- pop_t[[k]][i, 12] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 16:20) {
          pop_t[[k]][i, 13] <- pop_t[[k]][i, 13] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 21:25) {
          pop_t[[k]][i, 14] <- pop_t[[k]][i, 14] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 26:30) {
          pop_t[[k]][i, 15] <- pop_t[[k]][i, 15] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 31:35) {
          pop_t[[k]][i, 16] <- pop_t[[k]][i, 16] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 36:40) {
          pop_t[[k]][i, 17] <- pop_t[[k]][i, 17] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 41:45) {
          pop_t[[k]][i, 18] <- pop_t[[k]][i, 18] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 46:50) {
          pop_t[[k]][i, 19] <- pop_t[[k]][i, 19] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 51:55) {
          pop_t[[k]][i, 20] <- pop_t[[k]][i, 20] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 56:60) {
          pop_t[[k]][i, 21] <- pop_t[[k]][i, 21] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 61:65) {
          pop_t[[k]][i, 22] <- pop_t[[k]][i, 22] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 66:70) {
          pop_t[[k]][i, 23] <- pop_t[[k]][i, 23] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 71:75) {
          pop_t[[k]][i, 24] <- pop_t[[k]][i, 24] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 76:80) {
          pop_t[[k]][i, 25] <- pop_t[[k]][i, 25] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 81:85) {
          pop_t[[k]][i, 26] <- pop_t[[k]][i, 26] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 86:90) {
          pop_t[[k]][i, 27] <- pop_t[[k]][i, 27] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 91:95) {
          pop_t[[k]][i, 28] <- pop_t[[k]][i, 28] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 96:100) {
          pop_t[[k]][i, 29] <- pop_t[[k]][i, 29] + pop_s[[k]][[i]][j, 2]
        }
        for (j in 101:101) {
          pop_t[[k]][i, 30] <- pop_t[[k]][i, 30] + pop_s[[k]][[i]][j, 2]
        }
      }
      pop_t[[k]]$year <- c(1:project_year) - 1
      pop_t[[k]] <- pop_t[[k]][, c(31, 1:30)]
    }
    return(pop_t)
  }

  if (sex == "female") {
    for (k in 1:6) {
      for (i in 1:project_year) {
        pop_t[[k]][i, 2] <- pop_s[[k]][[i]][1, 3]
        pop_t[[k]][i, 3] <- round(cumsum(pop_s[[k]][[i]][, 5])[age_group], 0)
        pop_t[[k]][i, 1] <- round(cumsum(pop_s[[k]][[i]][, 3])[age_group], 0)

        for (j in 1:15) {
          pop_t[[k]][i, 4] <- pop_t[[k]][i, 4] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 16:60) {
          pop_t[[k]][i, 5] <- pop_t[[k]][i, 5] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 16:65) {
          pop_t[[k]][i, 6] <- pop_t[[k]][i, 6] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 61:age_group) {
          pop_t[[k]][i, 7] <- pop_t[[k]][i, 7] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 66:age_group) {
          pop_t[[k]][i, 8] <- pop_t[[k]][i, 8] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 81:age_group) {
          pop_t[[k]][i, 9] <- pop_t[[k]][i, 9] + pop_s[[k]][[i]][j, 3]
        }

        for (j in 1:5) {
          pop_t[[k]][i, 10] <- pop_t[[k]][i, 10] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 6:10) {
          pop_t[[k]][i, 11] <- pop_t[[k]][i, 11] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 11:15) {
          pop_t[[k]][i, 12] <- pop_t[[k]][i, 12] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 16:20) {
          pop_t[[k]][i, 13] <- pop_t[[k]][i, 13] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 21:25) {
          pop_t[[k]][i, 14] <- pop_t[[k]][i, 14] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 26:30) {
          pop_t[[k]][i, 15] <- pop_t[[k]][i, 15] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 31:35) {
          pop_t[[k]][i, 16] <- pop_t[[k]][i, 16] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 36:40) {
          pop_t[[k]][i, 17] <- pop_t[[k]][i, 17] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 41:45) {
          pop_t[[k]][i, 18] <- pop_t[[k]][i, 18] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 46:50) {
          pop_t[[k]][i, 19] <- pop_t[[k]][i, 19] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 51:55) {
          pop_t[[k]][i, 20] <- pop_t[[k]][i, 20] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 56:60) {
          pop_t[[k]][i, 21] <- pop_t[[k]][i, 21] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 61:65) {
          pop_t[[k]][i, 22] <- pop_t[[k]][i, 22] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 66:70) {
          pop_t[[k]][i, 23] <- pop_t[[k]][i, 23] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 71:75) {
          pop_t[[k]][i, 24] <- pop_t[[k]][i, 24] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 76:80) {
          pop_t[[k]][i, 25] <- pop_t[[k]][i, 25] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 81:85) {
          pop_t[[k]][i, 26] <- pop_t[[k]][i, 26] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 86:90) {
          pop_t[[k]][i, 27] <- pop_t[[k]][i, 27] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 91:95) {
          pop_t[[k]][i, 28] <- pop_t[[k]][i, 28] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 96:100) {
          pop_t[[k]][i, 29] <- pop_t[[k]][i, 29] + pop_s[[k]][[i]][j, 3]
        }
        for (j in 101:101) {
          pop_t[[k]][i, 30] <- pop_t[[k]][i, 30] + pop_s[[k]][[i]][j, 3]
        }
      }
      pop_t[[k]]$year <- c(1:project_year) - 1
      pop_t[[k]] <- pop_t[[k]][, c(31, 1:30)]
    }
    return(pop_t)
  }
}
