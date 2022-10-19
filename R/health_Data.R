#' Parameters of 5-state Cox Regression Model estimated from US HRS
#'
#' A dataset containing the parameters of the 5-state static, trend and frailty cox regression
#' models estimated with the 1998-2014 US Health and Retirement Study data.
#'
#' @format A data frame with 15 rows and 14 variables:
#' \describe{
#'    \item{Model}{static, trend or frailty}
#'    \item{Coef}{the covariate that the coefficient is corresponding to}
#'    \item{trans1}{parameter estimate for H to M transition}
#'    \item{trans2}{parameter estimate for H to D transition}
#'    \item{trans3}{parameter estimate for H to MD transition}
#'    \item{trans4}{parameter estimate for H to Dead transition}
#'    \item{trans5}{parameter estimate for M to MD transition}
#'    \item{trans6}{parameter estimate for M to Dead transition}
#'    \item{trans7}{parameter estimate for D to H transition}
#'    \item{trans8}{parameter estimate for D to M transition}
#'    \item{trans9}{parameter estimate for D to MD transition}
#'    \item{trans10}{parameter estimate for D to Dead transition}
#'    \item{trans11}{parameter estimate for MD to M transition}
#'    \item{trans12}{parameter estimate for MD to Dead transition}
#' }
#' @source \url{https://www.cepar.edu.au/publications/working-papers/multi-state-model-functional-disability-and-health-status-presence-systematic-trend-and-uncertainty}
"US_HRS_5"

#' Parameters of 5-state Cox Regression Model estimated from US HRS
#'
#' A dataset containing the parameters of the 5-state static cox regression
#' model estimated with the 1998-2014 US Health and Retirement Study data.
#' The rows are beta, gamma_age, gamma_f.
#'
#' @format A data frame with 3 rows and 12 variables:
#' \describe{
#'    \item{trans1}{parameter estimate for H to M transition}
#'    \item{trans2}{parameter estimate for H to D transition}
#'    \item{trans3}{parameter estimate for H to MD transition}
#'    \item{trans4}{parameter estimate for H to Dead transition}
#'    \item{trans5}{parameter estimate for M to MD transition}
#'    \item{trans6}{parameter estimate for M to Dead transition}
#'    \item{trans7}{parameter estimate for D to H transition}
#'    \item{trans8}{parameter estimate for D to M transition}
#'    \item{trans9}{parameter estimate for D to MD transition}
#'    \item{trans10}{parameter estimate for D to Dead transition}
#'    \item{trans11}{parameter estimate for MD to M transition}
#'    \item{trans12}{parameter estimate for MD to Dead transition}
#' }
#' @source \url{https://www.cepar.edu.au/publications/working-papers/multi-state-model-functional-disability-and-health-status-presence-systematic-trend-and-uncertainty}
"params_5_static"

#' Parameters of 5-state Cox Regression Model estimated from US HRS
#'
#' A dataset containing the parameters of the 5-state trend cox regression
#' model estimated with the 1998-2014 US Health and Retirement Study data.
#' The rows are beta, gamma_age, gamma_f, phi.
#'
#' @format A data frame with 4 rows and 12 variables:
#' \describe{
#'    \item{trans1}{parameter estimate for H to M transition}
#'    \item{trans2}{parameter estimate for H to D transition}
#'    \item{trans3}{parameter estimate for H to MD transition}
#'    \item{trans4}{parameter estimate for H to Dead transition}
#'    \item{trans5}{parameter estimate for M to MD transition}
#'    \item{trans6}{parameter estimate for M to Dead transition}
#'    \item{trans7}{parameter estimate for D to H transition}
#'    \item{trans8}{parameter estimate for D to M transition}
#'    \item{trans9}{parameter estimate for D to MD transition}
#'    \item{trans10}{parameter estimate for D to Dead transition}
#'    \item{trans11}{parameter estimate for MD to M transition}
#'    \item{trans12}{parameter estimate for MD to Dead transition}
#' }
#' @source \url{https://www.cepar.edu.au/publications/working-papers/multi-state-model-functional-disability-and-health-status-presence-systematic-trend-and-uncertainty}
"params_5_trend"

#' Parameters of 5-state Cox Regression Model estimated from US HRS
#'
#' A dataset containing the parameters of the 5-state frailty cox regression
#' model estimated with the 1998-2014 US Health and Retirement Study data.
#' The rows are beta, gamma_age, gamma_f, phi, alpha.
#'
#' @format A data frame with 5 rows and 12 variables:
#' \describe{
#'    \item{trans1}{parameter estimate for H to M transition}
#'    \item{trans2}{parameter estimate for H to D transition}
#'    \item{trans3}{parameter estimate for H to MD transition}
#'    \item{trans4}{parameter estimate for H to Dead transition}
#'    \item{trans5}{parameter estimate for M to MD transition}
#'    \item{trans6}{parameter estimate for M to Dead transition}
#'    \item{trans7}{parameter estimate for D to H transition}
#'    \item{trans8}{parameter estimate for D to M transition}
#'    \item{trans9}{parameter estimate for D to MD transition}
#'    \item{trans10}{parameter estimate for D to Dead transition}
#'    \item{trans11}{parameter estimate for MD to M transition}
#'    \item{trans12}{parameter estimate for MD to Dead transition}
#' }
#' @source \url{https://www.cepar.edu.au/publications/working-papers/multi-state-model-functional-disability-and-health-status-presence-systematic-trend-and-uncertainty}
"params_5_frailty"

#' Parameters of 3-state Cox Regression Model estimated from US HRS
#'
#' A dataset containing the parameters of the 3-state static, trend and frailty cox regression
#' models estimated with the 1998-2014 US Health and Retirement Study data.
#'
#' @format A data frame with 15 rows and 6 variables:
#' \describe{
#'    \item{Model}{static, trend or frailty}
#'    \item{Coef}{the covariate that the coefficient is corresponding to}
#'    \item{trans1}{parameter estimate for H to M transition}
#'    \item{trans2}{parameter estimate for H to D transition}
#'    \item{trans3}{parameter estimate for H to MD transition}
#'    \item{trans4}{parameter estimate for H to Dead transition}
#' }
#' @source \url{https://cepar.edu.au/publications/working-papers/functional-disability-systematic-trends-and-uncertainty-comparison-between-china-and-us}
"US_HRS"

#' Parameters of 3-state Cox Regression Model estimated from CLHLS
#'
#' A dataset containing the parameters of the 3-state static, trend and frailty cox regression
#' models estimated with the 1998-2014 CLHLS data.
#'
#' @format A data frame with 15 rows and 6 variables:
#' \describe{
#'    \item{Model}{static, trend or frailty}
#'    \item{Coef}{the covariate that the coefficient is corresponding to}
#'    \item{trans1}{parameter estimate for H to M transition}
#'    \item{trans2}{parameter estimate for H to D transition}
#'    \item{trans3}{parameter estimate for H to MD transition}
#'    \item{trans4}{parameter estimate for H to Dead transition}
#' }
#' @source \url{https://cepar.edu.au/publications/working-papers/functional-disability-systematic-trends-and-uncertainty-comparison-between-china-and-us}
"china_CLHLS"

#' 10000 simulated paths for males aged 65 in 2012 initially healthy under the 5-state trend model
#'
#' A matrix containing 10000 simulated paths for males aged 65 in 2012 initially healthy under the 5-state trend model with parameters estimated with US HRS data
#' Each row of the matrix represent the path of one individual from age 65 to 110
#' Used as an inbuilt variable to illustrate the statistics functions for the 5-state health model
#'
"health5_simulated_path_example"
