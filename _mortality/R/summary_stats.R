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


cohort2period <- function(cohort_rates, ages) {

  c2p_mat <- function(c_rates) {

    # Convert vector to matrix if necessary
    c_mat <- as.matrix(c_rates)

    r <- nrow(c_mat)
    c <- ncol(c_mat)

    p_rates <- matrix(NA, nrow = r, ncol = c)

    for (i in 1:r) {

      # Check for 0 as tail does not handle 0 well
      if (ages[i] == 0) {
        p_rates[i, ] <- c_mat[i, ]
        next
      }

      if (ages[i] < c) {
        p_vec <- c(rep(NA, ages[i]), utils::head(c_mat[i, ], -ages[i]))
        p_rates[i, ] <- p_vec
      } else break
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


#' Summarise Curtate Future Lifetime Statistics
#'
#' Produces expected value and variance of curtate future lifetime for a life table.
#'
#' @param qx
#' vector, matrix or 3D array of 1-year death probabilities with age
#' (on the rows) and calendar year (on the columns) and simulation
#' number (3rd dimension)
#' @param ages
#' vector of ages for `qx`
#' @param init_age
#' initial age for which the curtate future lifetime is to be calculated at. If not provided,
#' the summary statistics will be calculated for the smallest age supplied in `ages`
#' @param years
#' optional vector of years for `qx`. If not supplied, then the column names
#' of `qx` will be preserved
#'
#' @return
#' expected value and variance of curtate future lifetime as a 3D array
#' if `qx` is an array, or as a matrix otherwise
#' @export
#'
#' @examples
#'
summarise_cfl <- function(qx, ages, init_age = NULL, years = NULL) {
  if(is.null(init_age)) {
    init_age <- ages[1]
  } else if (!is.element(init_age, ages)) {
    stop("invalid initial age")
  }

  # Converting to 1-year survival probabilities
  if(init_age == ages[1]) {
    px <- 1 - qx
  } else {
    px <- 1 - utils::tail(qx, ages[1] - init_age)
  }

  # Calculating kpx
  if (is.vector(px)) {
    kpx <- matrix(cumprod(px))
  } else if (is.matrix(px)) {
    kpx <- apply(px, 2, cumprod)
  } else if (is.array(px)) {
    kpx <- arr_apply(px, function(x) apply(x, 2, cumprod))
  }

  # Changing dim names
  stopifnot(dim(px) == dim(kpx))
  colnames(kpx) <- colnames(px)
  k <- 1:nrow(kpx)
  rownames(kpx) <- as.character(k)

  summarise_cfl_mat <- function(kpx_mat) {
    # Expected curtate future lifetime
    exp_cfl <- apply(kpx_mat, 2, sum)
    # Assumes kpx has been given until terminal age

    rownames(exp_cfl) <- NULL
    # 2nd moment of curtate future lifetime
    moment_2 <- apply(2 * k * kpx_mat, 2, sum) - exp_cfl

    # Variance of curtate future lifetime
    var_cfl <- moment_2 - (exp_cfl)^2

    # Years on columns, stats on rows
    result_mat <- rbind(exp_cfl, var_cfl)

    return(result_mat)
  }

  # kpx should be matrix or array, note that is.array(A) = TRUE where A is matrix
  stopifnot(is.array(kpx))
  if (is.matrix(kpx)) {
    result <- summarise_cfl_mat(kpx)
  } else {
    result <- arr_apply(kpx, summarise_cfl_mat)
  }

  colnames(result) <- if (is.null(years)) colnames(qx) else as.character(years)

  return(result)

}





