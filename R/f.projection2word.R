#' Title
#'
#' @param path_out_table
#' @param path_out_pic
#' @param path_out
#' @param initial_year
#' @param x_by
#' @param area_name
#' @param pop_name
#'
#' @return
#' @export
#'
#' @examples
f.projection2word <- function(path_out_table, path_out_pic, path_out,
                              initial_year = 2020, x_by = 5,
                              area_name, pop_name) {
  project_p <- read_pop.t(path_out_table) # 1. Total Data

  project_year <- length(project_p[[1]][[1]][, 1])
  project_p_s <- read_pop.s(path_out_table, project_year) # 2. Structure Data

  # 3. Table Data
  p1 <- project_table(initial_year, project_year, project_p[[1]])
  p1.s <- project_female_15t49_table(initial_year, project_year, project_p[[1]], project_p_s[[1]])
  p1[[12]] <- p1.s
  rm(p1.s)

  p2 <- project_table(initial_year, project_year, project_p[[2]])
  p2.s <- project_female_15t49_table(initial_year, project_year, project_p[[2]], project_p_s[[2]])
  p2[[12]] <- p2.s
  rm(p2.s)

  # 4. Output Data
  project_pic(p1, initial_year, project_year, x_by = x_by, path_out_pic[1])
  project_pic(p2, initial_year, project_year, x_by = x_by, path_out_pic[2])

  project_word_2pop(
    dt1 = p1, dt2 = p2, path_out_pic = path_out_pic, path_out = path_out,
    initial_year = initial_year, project_year = project_year,
    area_name = area_name, pop_name = pop_name
  )
}
