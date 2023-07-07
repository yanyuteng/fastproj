#' Urbanization Rate trans to Year over Year Growth
#' @description Urbanization rate needs to be converted into an increase rate or decrease rate.
# 'This function assumes that, first, the urbanization rate of each Scenario is the same, and second, the proportion of newly urbanized or reduced rural population is the same for all age groups,
# 'That is, although the projection parameters are consistent, there will still be differences between urban and rural results due to differences in the number of people of different initial ages in urban and rural areas.
# 'Of course, more assumptions can be introduced in the future. First, the urbanization rate of different scenarios is different, and second, the urbanization rate is distributed differently in different age groups.
#' @param dt
#' @param type
#'
#' @return
#' @export
#'
#' @examples
yoy_growth <- function(dt, type = 1) {
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
