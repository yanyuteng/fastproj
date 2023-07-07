#' Plot Projection Population
#'
#' @param dt
#' @param initial_year
#' @param project_year
#' @param x_by
#' @param path_out_pic
#'
#' @return
#' @export
#'
#' @examples
project_pic <- function(dt, initial_year, project_year, x_by, path_out_pic) {
  if (dir.exists(path_out_pic) == TRUE) {
    unlink(path_out_pic, recursive = TRUE)
    dir.create(path_out_pic)
  }
  if (dir.exists(path_out_pic) == FALSE) {
    dir.create(path_out_pic)
  }
  year <- seq(initial_year, initial_year + project_year, by = x_by)
  ppi <- 300
  png(paste0(path_out_pic, "/p_%d.png", ""), width = 4 * ppi, height = 3 * ppi, res = ppi)

  # 1. 总人数
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[1]][, 2] / 10000, colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[1]][, 2] / 10000), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[1]][, 3] / 10000, colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[1]][, 3] / 10000), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[1]][, 4] / 10000, colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[1]][, 4] / 10000), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "人数(万)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) + # 按变量名对应
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 2. 出生人数
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[2]][, 2] / 10000, colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[2]][, 2] / 10000), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[2]][, 4] / 10000, colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[2]][, 4] / 10000), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[2]][, 6] / 10000, colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[2]][, 6] / 10000), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "人数(万)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 3. 出生率
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[2]][, 3], colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[2]][, 3]), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[2]][, 5], colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[2]][, 5]), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[2]][, 7], colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[2]][, 7]), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "百分比(%)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 4. 自然增长率
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[4]][, 3], colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[4]][, 3]), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[4]][, 5], colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[4]][, 5]), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[4]][, 7], colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[4]][, 7]), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "百分比(%)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 5. 0-14少儿人口比重
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[5]][, 3], colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[5]][, 3]), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[5]][, 5], colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[5]][, 5]), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[5]][, 7], colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[5]][, 7]), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "百分比(%)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 6. 15-64劳动人口比重
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[7]][, 3], colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[7]][, 3]), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[7]][, 5], colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[7]][, 5]), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[7]][, 7], colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[7]][, 7]), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "百分比(%)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 7. 65及以上老年人口比重
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[9]][, 3], colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[9]][, 3]), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[9]][, 5], colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[9]][, 5]), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[9]][, 7], colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[9]][, 7]), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "百分比(%)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  # 8. 15-49育龄妇女人口数量
  print(
    ggplot() +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[12]][, 3], colour = "低方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[12]][, 3]), colour = "green", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[12]][, 5], colour = "中方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[12]][, 5]), colour = "red", group = 2) +
      geom_point(aes(x = dt[[1]][, 1], y = dt[[12]][, 7], colour = "高方案"), size = 1, shape = 1) +
      geom_line(aes(x = dt[[1]][, 1], y = dt[[12]][, 7]), colour = "blue", group = 2) +
      scale_x_continuous(breaks = year) +
      labs(x = "年份", y = "百分比(%)", colour = "方案") +
      scale_color_discrete(breaks = c("高方案", "中方案", "低方案")) +
      theme_bw() +
      theme(text = element_text(family = "STFangsong")) +
      theme(plot.title = element_text(hjust = 0.5))
  )

  dev.off()
}
