#' Parameters of Cox Regression Model estimated from US HRS Study
#'
#' A dataset containing the parametets of the static, trend and frailty cox regression
#' models estimated in the 1998-2014 US Health and Retirement Study. The model includes
#' parameters for the following covariates: age, gender, time (trend), stochastic latent factor
#' (frailty).
#'
#' @format A tibble OR data frame with 15 rows and 5 columns:
#' \describe{
#'    \item{Model}{static, trend or frailty}
#'    \item{Coef}{the covariate that the coefficient is corresponding to}
#'    \item{trans1}{parameter estimate for H to F transition}
#'    \item{trans2}{parameter estimate for F to H transition}
#'    \item{trans3}{parameter estimate for H to D transition}
#'    \item{trans4}{parameter estimate for F to D transition}
#' }
#'
#' @source \url{https://cepar.edu.au/publications/working-papers/functional-disability-systematic-trends-and-uncertainty-comparison-between-china-and-us}
'US_HRS'
