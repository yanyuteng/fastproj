#' Project Mortality
#' @description Using the semi-approximate solution method, the function is rewritten from Wang Guangzhou.
#' @param ex_p Life Expectancy Parameters
#' @param lx_p Lx Parameters
#' @param axn axn Parameters
#' @param age_group age group
#'
#' @return
#' @export
#'
#' @examples
Lx_p <- function(ex_p, lx_p, axn, age_group = 101) { # ex_0, mx_max_age
  epslon <- 0.000001
  lx <- c()
  Lx <- c()
  Tx <- c()
  ex <- c()
  arfa_min <- -100
  arfa_max <- 100
  current_e0 <- 0
  male_arfa <- 0
  keys <- 100

  while (keys >= epslon) {
    arfa <- (arfa_max + arfa_min) / 2
    current_e0 <- 0

    lx_p[1] <- 1
    for (i in 2:age_group) {
      lx[i] <- lx_p[i] / (lx_p[i] + (1 - lx_p[i]) * exp(2 * arfa))
    }

    lx[1] <- 1
    for (i in 1:age_group - 1) {
      Lx[i] <- lx[i + 1] + axn[i] * (lx[i] - lx[i + 1])
    }
    Lx[age_group] <- lx[age_group] + (lx[age_group] - lx[age_group - 1]) / 2
    # Open age_group Modified from Wang Guangzhou (2020)

    for (i in 1:age_group) {
      temp <- 0
      for (j in i:age_group) {
        temp <- temp + Lx[j]
        Tx[i] <- temp
      }
      ex[i] <- Tx[i] / lx[i]
    }

    current_e0 <- ex[1]

    if (current_e0 < ex_p) {
      arfa_max <- arfa
      keys <- abs(ex_p - current_e0)
    } else {
      arfa_min <- arfa
      keys <- abs(ex_p - current_e0)
    }
  }
  return(Lx)
}
