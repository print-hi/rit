#' Distribution of Survival Function
#'
#' Distribution of the Survival Function
#'
#' Linear interpolation is performed between the discrete points of the survival function
#'
#' @param surviv_rates
#' vector of survival rates
#' @param surviv_time
#' vector of survival times
#'
#' @return
#' Distribution function
#'
#' @export
#'
#' @examples
#'
psurviv <- function(surviv_rates, surviv_time) {

  n <- length(surviv_rates)
  # Linear interpolation
  surviv_fun <- stats::approxfun(0:(n - 1), surviv_rates)

  surviv_fun(surviv_time)

}
