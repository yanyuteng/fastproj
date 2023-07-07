#' Build a life table
#'
#' @param age_name age group
#' @param nMx mortality
#'
#' @return a dataframe
#' @export
#'
#' @examples
lt_1year <- function(age_name, nMx) {
  b0 <- 0.07
  b1 <- 1.7
  nmax <- length(age_name)
  n <- c(diff(age_name), 999) # width of the intervals
  nax <- n / 2 # default to .5 of interval
  nax[1] <- b0 + b1 * nMx[1] # from Keyfitz & Flieger(1968)
  # nax[2] <- 1.5  ;
  nax[nmax] <- 1 / nMx[nmax] # e_x at open age interval
  nqx <- (n * nMx) / (1 + (n - nax) * nMx)
  nqx <- ifelse(nqx > 1, 1, nqx) # necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1, cumprod(1 - nqx))  # survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx
  # nLx <- n*lx - nax*ndx;       		# equivalent to n*l(x+n) + (n-nax)*ndx
  nLx <- n * lx - (n - nax) * ndx # equivalent to n*l(x+n) + nax*ndx
  nLx[nmax] <- lx[nmax] * nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse(lx[1:nmax] > 0, Tx / lx[1:nmax], NA)
  lt <- data.frame(age_name = age_name, nax = nax, nmx = nMx, nqx = nqx, lx = lx, ndx = ndx, nLx = nLx, Tx = Tx, ex = ex)
  return(lt)
}
