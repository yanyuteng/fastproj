% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx_modify.R
\name{lx_modify}
\alias{lx_modify}
\title{Modify Mortality}
\usage{
lx_modify(q0_5_m, e0_m, q0_5_f, e0_f, method = "wilmoth")
}
\arguments{
\item{method}{'Wilmoth'}
}
\value{
a list
}
\description{
Wilmoth method code from 'DemoTools'
}
\examples{
inparm_lt = lx_modify(q0_5_m = 0.00318, e0_m = 76.75, q0_5_f = 0.00264, e0_f = 82.22)
#inparm_lt = list(lt_1year(c(0:100),infile_pop[[1]][,4]),lt_1year(c(0:100),infile_pop[[1]][,5]))
}
