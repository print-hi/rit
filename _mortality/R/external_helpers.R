#' Combine Historical and Simulated Rates
#'
#' @param rates_hist
#' Matrix of historical mortality rates with age (on the rows) and
#' calendar year or cohort (on the columns)
#' @param rates_sim
#' 3D array of simulated mortality rates with age (on the rows) and
#' calendar year or cohort (on the columns) and simulation number (3rd dimension)
#'
#' @return
#' 3D array of combined historical and simulated rates
#' @export
#'
#' @examples
#'
combine_hist_sim <- function(rates_hist, rates_sim) {

# Flagging errors ---------------------------------------------------------

  if (!is.matrix(rates_hist)) {
    stop("historical rates must be a 2D matrix")
  }

  if (!(is.array(rates_sim) & length(dim(rates_sim)) == 3)) {
    stop("simulated rates must be a 3D array")
  }

  if (dim(rates_hist)[1] != dim(rates_sim)[1]) {
    stop("historical and simulated rates must have an equal number of rows")
  }


# Implementation ----------------------------------------------------------

  n_row = dim(rates_hist)[1]
  n_col = dim(rates_hist)[2] + dim(rates_sim)[2]
  n_sim = dim(rates_sim)[3]


  rates_all <- array(NA, dim = c(n_row, n_col, n_sim))
  for (i in 1:n_sim) {
    rates_all[,,i] = cbind(rates_hist, rates_sim[,,i])
  }

  rownames(rates_all) <- rownames(rates_hist)
  colnames(rates_all) <- c(colnames(rates_hist), colnames(rates_sim))

  rates_all
}



period2cohort <- function(period_rates, ages, init_age = NULL) {

  # Requires years to be continuous increasing vector e.g. 2000:2017

  if(is.null(init_age)) {
    init_age <- ages[1]
  } else if (!is.element(init_age, ages)) {
    stop("initial age must be in ages")
  }

  # Extract relevant rates starting from init_age
  if(init_age != ages[1]) {
    period_rates <- utils::tail(period_rates, ages[1] - init_age)
  }

  p2c_mat <- function(p_rates) {

    # Convert vector to matrix if necessary
    p_mat <- as.matrix(p_rates)

    n_row <- nrow(p_mat)
    n_col <- ncol(p_mat)

    c_rates <- matrix(NA, nrow = n_row, ncol = n_col)

    # Filling out cohort rates
    for (i in 1:min(n_row, n_col)) {
      c_rates[i, ] <- c(p_mat[i, i:n_col], rep(NA, i - 1))
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


cohort2period <- function(cohort_rates) {

  c2p_mat <- function(c_rates) {

    # Convert vector to matrix if necessary
    c_mat <- as.matrix(c_rates)

    n_row <- nrow(c_mat)
    n_col <- ncol(c_mat)

    p_rates <- matrix(NA, nrow = n_row, ncol = n_col)

    # Filling out period rates
    for (i in 1:min(n_row, n_col)) {
      p_rates[i, ] <- c(rep(NA, i - 1), c_mat[i, 1:(n_col + 1 - i)])
    }

    p_rates

  }

  if (is.vector(cohort_rates) | is.matrix(cohort_rates)) {
    period_rates <- c2p_mat(cohort_rates)
  } else if (is.array(cohort_rates)) {
    period_rates <- arr_apply(cohort_rates, c2p_mat)
  }

  dimnames(period_rates) <- dimnames(cohort_rates)

  period_rates

}



