#' Title
#'
#' @param project_year
#'
#' @return
#' @export
#'
#' @examples
pop_total_initial_scene <- function(project_year) {
  pop_temp <- list()
  temp <- data.frame(matrix(0, nrow = project_year, ncol = 30))
  colnames(temp) <- c(
    "pop_total",
    "birth", "death",
    "t0_14", "t15_59", "t15_64", "t60plus", "t65plus", "t80plus",
    "t0_4", "t5_9", "t10_14",
    "t15_19", "t20_24", "t25_29", "t30_34", "t35_39", "t40_44", "t45_49",
    "t50_54", "t55_59",
    "t60_64", "t65_69", "t70_74", "t75_79",
    "t80_84", "t85_89", "t90_94", "t95_99", "t100"
  )
  for (i in 1:6) {
    pop_temp[[i]] <- temp
  }
  return(pop_temp)
}
