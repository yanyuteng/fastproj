#' Title
#'
#' @param dt1
#' @param dt2
#' @param path_out_pic
#' @param path_out
#' @param initial_year
#' @param project_year
#' @param area_name
#' @param pop_name
#'
#' @return
#' @export
#'
#' @examples
project_word_2pop <- function(dt1, dt2, path_out_pic, path_out,
                              initial_year, project_year, area_name, pop_name) {
  library(flextable)
  library(officer)

  end_year <- initial_year + project_year - 1 # Information

  set_flextable_defaults(
    font.family = "STFangsong", font.size = 12,
    digits = 2
  )

  # 0. Write Heading 1
  docx <- read_docx()
  docx <- body_add(docx, "人口预测主要结果", style = "Normal") # heading 1
  docx <- body_add_par(docx, "")


  # 1. Write Heading 2
  docx <- body_add(docx, "1. 人口规模", style = "Normal")
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表1", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "人口数量"), style = "centered")
  t1a <- flextable(dt1[[1]]) %>%
    # set_caption(caption = paste0('表1',' ',initial_year,'-',end_year,'年',area_name,pop_name[1],'人口数量')) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t1a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表2", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "人口数量"), style = "centered")
  t1b <- flextable(dt2[[1]]) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t1b)
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_1.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图1", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "人口数量"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_1.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图2", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "人口数量"), style = "centered")
  docx <- body_add_par(docx, "")


  # 2. Write Heading 2
  docx <- body_add(docx, "2. 出生人口与死亡人口", style = "Normal")
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表3", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "出生人口和出生率"), style = "centered")
  t2a <- flextable(dt1[[2]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t2a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表4", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "出生人口和出生率"), style = "centered")
  t2b <- flextable(dt2[[2]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t2b)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表5", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "死亡人口和死亡率"), style = "centered")
  t3a <- flextable(dt1[[3]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t3a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表6", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "死亡人口和死亡率"), style = "centered")
  t3b <- flextable(dt2[[3]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t3b)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表7", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "自然增长人口和自然增长率"), style = "centered")
  t4a <- flextable(dt1[[4]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t4a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表8", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "自然增长人口和自然增长率"), style = "centered")
  t4b <- flextable(dt2[[4]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t4b)
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_2.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图3", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "出生人口"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_2.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图4", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "出生人口"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_3.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图5", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "人口出生率"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_3.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图6", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "人口出生率"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_4.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图7", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "人口自然增长率"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_4.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图8", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "人口自然增长率"), style = "centered")
  docx <- body_add_par(docx, "")


  # 3. Write Heading 2
  docx <- body_add(docx, "3. 15岁至64岁劳动力人口", style = "Normal")
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表9", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "15-64岁人口比重"), style = "centered")
  t7a <- flextable(dt1[[7]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t7a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表10", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "15-64岁人口比重"), style = "centered")
  t7b <- flextable(dt2[[7]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t7b)
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_6.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图9", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "15-64岁劳动力人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_6.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图10", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "15-64岁劳动力人口比重"), style = "centered")
  docx <- body_add_par(docx, "")


  # 4. Write Heading 2
  docx <- body_add(docx, "4. 少儿人口与老年人口", style = "Normal")
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表11", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "0-14岁岁人口比重"), style = "centered")
  t5a <- flextable(dt1[[5]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t5a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表12", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "0-14岁岁人口比重"), style = "centered")
  t5b <- flextable(dt2[[5]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t5b)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表13", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "65岁及以上人口比重"), style = "centered")
  t9a <- flextable(dt1[[9]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t9a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表14", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "65岁及以上人口比重"), style = "centered")
  t9b <- flextable(dt2[[9]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t9b)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表15", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "80岁及以上人口比重"), style = "centered")
  t10a <- flextable(dt1[[10]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t10a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表16", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "80岁及以上人口比重"), style = "centered")
  t10b <- flextable(dt2[[10]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t10b)
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_5.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图11", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "0-14岁少儿人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_5.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图12", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "0-14岁少儿人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_7.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图13", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "65岁及以上老年人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_7.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图14", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "65岁及以上老年人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表17", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "少儿抚养比与老年抚养比"), style = "centered")
  t11a <- flextable(dt1[[11]]) %>%
    add_header_row(top = TRUE, values = c("", "少儿抚养比", "老年抚养比"), colwidths = c(1, 3, 3)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t11a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表18", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "少儿抚养比与老年抚养比"), style = "centered")
  t11b <- flextable(dt2[[11]]) %>%
    add_header_row(top = TRUE, values = c("", "少儿抚养比", "老年抚养比"), colwidths = c(1, 3, 3)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t11b)
  docx <- body_add_par(docx, "")

  # 5. Write Heading 2
  docx <- body_add(docx, "5. 育龄妇女人口", style = "Normal")
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表19", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "15-49岁育龄妇女人口比重"), style = "centered")
  t12a <- flextable(dt1[[12]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t12a)
  docx <- body_add_par(docx, "")

  docx <- body_add(docx, paste0("表20", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "15-49岁育龄妇女人口比重"), style = "centered")
  t12b <- flextable(dt2[[12]]) %>%
    add_header_row(top = TRUE, values = c("", "低方案", "中方案", "高方案"), colwidths = c(1, 2, 2, 2)) %>%
    set_table_properties(layout = "autofit")
  docx <- body_add_flextable(x = docx, value = t12b)
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[1], "/p_8.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图15", " ", initial_year, "-", end_year, "年", area_name, pop_name[1], "15-49岁育龄妇女人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  docx <- body_add_img(
    x = docx,
    src = paste0(path_out_pic[2], "/p_8.png"),
    height = 3, width = 4.3,
    style = "centered"
  )
  docx <- body_add(docx, paste0("图16", " ", initial_year, "-", end_year, "年", area_name, pop_name[2], "15-49岁育龄妇女人口比重"), style = "centered")
  docx <- body_add_par(docx, "")

  print(docx, paste0(path_out, "/projection_outcome.docx"))
}
