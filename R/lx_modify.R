#' Title
#'
#' @param q0_5_m
#' @param e0_m
#' @param q0_5_f
#' @param e0_f
#' @param method
#'
#' @return
#' @export
#'
#' @examples
lx_modify <- function(q0_5_m, e0_m, q0_5_f, e0_f, method = "wilmoth") {
  qx_male <- lt_model_lq(Sex = "m", q0_5 = q0_5_m, e0 = e0_m)
  qx_female <- lt_model_lq(Sex = "f", q0_5 = q0_5_f, e0 = e0_f)

  lt_mod_male <- lt_abridged2single(
    nMx = qx_male[[1]]$nMx, Age = c(0, 1, seq(5, 110, by = 5)),
    axmethod = "pas", Sex = "m", mod = FALSE
  )
  lt_mod_female <- lt_abridged2single(
    nMx = qx_female[[1]]$nMx, Age = c(0, 1, seq(5, 110, by = 5)),
    axmethod = "pas", Sex = "f", mod = FALSE
  )

  lt_male <- lt_1year(c(0:100), lt_mod_male[1:101, 3])
  lt_female <- lt_1year(c(0:100), lt_mod_female[1:101, 3])

  inparm_lt <- list(lt_male, lt_female)
  return(inparm_lt)
}
