# urban 需要转化为增幅或降幅
# 本文处理程序假定，其一，各方案城市化率相同，其二，各年龄段新增城市化人口或减少农村人口比重一致，
# 即，尽管各预测参数一致，但由于城乡初始年龄别人口数量不同，城乡结果仍然会存在差异性
# 当然，未来可以引入更多的假设，其一，不同方案城市化率不同，其二，城市化率在不同年龄段分布不同
#' Title
#'
#' @param dt
#' @param type
#'
#' @return
#' @export
#'
#' @examples
mom_growth <- function(dt, type = 1) {
  pl <- length(dt)
  temp <- c()
  if (type == 1) {
    for (i in 1:(pl - 1)) {
      temp[i + 1] <- 1 + (dt[i + 1] - dt[i]) / dt[i]
    }
    temp[1] <- 1
    return(temp)
  }
  if (type == 2) {
    dt <- 100 - dt # 另一人群 此消彼长
    for (i in 1:(pl - 1)) {
      temp[i + 1] <- 1 + (dt[i + 1] - dt[i]) / dt[i]
    }
    temp[1] <- 1
    return(temp)
  }
}
