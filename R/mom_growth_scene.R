#' Title
#'
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
mom_growth_scene <- function(dt) {
  pl <- length(dt[[1]])
  temp <- c()
  temp2 <- list()

  for (k in 1:3) {
    for (i in 1:(pl - 1)) {
      temp[i + 1] <- 1 + (dt[[k]][i + 1] - dt[[k]][i]) / dt[[k]][i]
    }
    temp[1] <- 1
    temp2[[k]] <- temp
  }
  for (k in 4:6) {
    dt[[k]] <- 100 - dt[[k]] # 另一人群 此消彼长
    for (i in 1:(pl - 1)) {
      temp[i + 1] <- 1 + (dt[[k]][i + 1] - dt[[k]][i]) / dt[[k]][i]
    }
    temp[1] <- 1
    temp2[[k]] <- temp
  }

  return(temp2)
}
