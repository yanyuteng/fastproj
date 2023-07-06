#' Title
#'
#' @param initial_year
#' @param project_year
#' @param dt
#'
#' @return
#' @export
#'
#' @examples
project_female_15t49 <- function(initial_year = 2020, project_year = 1, dt) {
  temp <- data.frame(matrix(0, nrow = project_year, ncol = 2))
  colnames(temp) <- c("year", "female_15t49")
  temp[, 1] <- c(initial_year:(initial_year + project_year - 1))
  for (i in 1:project_year) {
    for (j in 16:50) {
      temp[i, 2] <- temp[i, 2] + dt[[i]][j, 3]
      # temp[i,2] = cumsum(dt[[1]][16:50,])[35,3]
    }
  }
  return(temp)
}
