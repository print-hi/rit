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


period2cohort <- function(period_rates, ages) {

  # Requires years to be continuous increasing vector e.g. 2000:2017

  # TODO: Cannot immediately switch from lifetable, need to convert into
  # survival probabilities
  # TODO: Return something suggesting it is cohort life table?

  p2c_mat <- function(p_rates) {

    # Convert vector to matrix if necessary
    p_mat <- as.matrix(p_rates)

    r <- nrow(p_mat)
    c <- ncol(p_mat)

    c_rates <- matrix(NA, nrow = r, ncol = c)

    for (i in 1:r) {

      # Check for 0 as tail does not handle 0 well
      if (ages[i] == 0) {
        c_rates[i, ] <- p_mat[i, ]
        next
      }

      if (ages[i] < c) {
        c_vec <- c(utils::tail(p_mat[i, ], -ages[i]), rep(NA, ages[i]))
        c_rates[i, ] <- c_vec
      } else break
    }

    c_rates
  }


  if (is.vector(period_rates) | is.matrix(period_rates)) {
    cohort_rates <- p2c_mat(period_rates)
  } else if (is.array(period_rates)) {
    cohort_rates <- arr_apply(period_rates, p2c_mat)
  }

  dimnames(cohort_rates) <- dimnames(period_rates)


  cohort_rates

}





