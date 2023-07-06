#' Title
#'
#' @param initial_year
#' @param project_year
#' @param dt
#' @param dt_list
#'
#' @return
#' @export
#'
#' @examples
project_female_15t49_table <- function(initial_year, project_year, dt, dt_list) {
  temp <- list()
  for (j in 1:3) {
    temp[[j]] <- project_female_15t49(initial_year, project_year, dt_list[[j]])
    temp[[j]] <- as.data.frame(lapply(temp[[j]], as.numeric))
  }
  temp <- data.frame(cbind(temp[[1]], temp[[2]][, -1], temp[[3]][, -1]))
  temp <- data.frame(cbind(
    temp[, 1],
    temp[, 2], round(temp[, 2] / dt[[1]][, 2] * 100, 2),
    temp[, 3], round(temp[, 3] / dt[[2]][, 2] * 100, 2),
    temp[, 4], round(temp[, 4] / dt[[3]][, 2] * 100, 2)
  ))[-1, ]
  colnames(temp) <- c(
    "年份",
    "低育龄妇女人数(人)", "低比重(%)",
    "中育龄妇女人数(人)", "中比重(%)",
    "高育龄妇女人数(人)", "高比重(%)"
  )
  return(temp)
}
