

#' The Survival Function
#'
#' Distribution and quantile function  of the survival function.
#'
#' Linear interpolation is performed between the discrete points of the survival function
#'
#' @param surviv_fun
#' vector of survival function
#' @param surviv_time
#' vector of survival times
#' @param surviv_prob
#' vector of survival probabilities
#'
#' @name surviv
NULL

#' @rdname surviv
#'
#' @export
psurviv <- function(surviv_fun, surviv_time) {

  n <- length(surviv_fun)

  # Checking bounds
  stopifnot(surviv_time >= 0)

  # Linear interpolation
  surviv_fun_approx <- stats::approxfun(0:(n - 1), surviv_fun)

  result <- surviv_fun_approx(surviv_time)
  # Note: returns NA for surviv_time >= n - 1

  return(result)

}

#' @rdname surviv
#'
#' @export
qsurviv <- function(surviv_fun, surviv_prob) {

  n <- length(surviv_fun)

  # Checking bounds
  stopifnot(surviv_prob >= 0, surviv_prob <= 1)

  # Linear interpolation
  surviv_fun_approx <- stats::approxfun(0:(n - 1), surviv_fun)

  # Creating list of linearly interpolated functions shifted by element of surviv_prob
  surviv_fun_list <- lapply(surviv_prob, function(p) stats::approxfun(0:(n - 1), surviv_fun - p))

  # Use uniroot to determine quantile
  sapply(surviv_fun_list, function(fn) uniroot(fn, c(0, (n - 1)))$root)

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
#' Produces expected curtate future lifetime for a life table.
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
<<<<<<< HEAD
#' expected curtate future lifetime as a matrix if `qx` is a 3D array
#' with simulation number (on the rows) and calendar year (on the columns).
#' Returns a vector otherwise
=======
#' expected curtate future lifetime as a matrix with calendar year (on the rows)
#' and simulation number (on the columns).
>>>>>>> d920b612790c17988a857f812da9946e42d2fb4d
#'
#' @export
#'
#' @examples
#'
exp_cfl <- function(qx, ages, init_age = NULL, years = NULL) {
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

  exp_cfl_mat <- function(kpx_mat) {
    # Expected curtate future lifetime
    # Assumes kpx has been given until terminal age

    return(apply(kpx_mat, 2, sum))
  }

  # kpx should be matrix or array, note that is.array(A) = TRUE where A is matrix
  stopifnot(is.array(kpx))
  if (is.matrix(kpx)) {
<<<<<<< HEAD
    result <- exp_cfl_mat(kpx)

    return(result)
  } else {
    result <- arr_apply(kpx, exp_cfl_mat)
    rownames(result) <- if (is.null(years)) colnames(qx) else as.character(years)

    return(t(result))
  }

}
=======
    result <- as.matrix(exp_cfl_mat(kpx))
  } else {
    result <- arr_apply(kpx, exp_cfl_mat)
  }

  rownames(result) <- if (is.null(years)) colnames(qx) else as.character(years)
>>>>>>> d920b612790c17988a857f812da9946e42d2fb4d

#' Plot Curtate Future Lifetime Forecasts
#'
#' Plots historical and forecasted expected curtate future lifetime.
#'
#' @param exp_cfl_hist
#' vector of expected curtate future lifetime for historical years
#' @param years_hist
#' vector of historical years
#' @param exp_cfl_for
#' matrix of expected curtate future lifetime for forecasted years
#' with simulation number (on the rows) and calendar year (on the columns)
#' @param years_for
#' vector of forecasted years
#' @param level
#' desired confidence level with 95% as default
#'
#' @return
#'
#' @export
#'
#' @examples
#'
plot_exp_cfl <- function(exp_cfl_hist, years_hist, exp_cfl_for, years_for, level = 95) {

  # Calculating mean, upper and lower values of confidence interval for simulations
  # of expected curtate future lifetime
  exp_cfl_mean <- apply(exp_cfl_for, 2, mean)
  exp_cfl_lower <- apply(exp_cfl_for, 2, quantile, 1 - level / 100)
  exp_cfl_upper <- apply(exp_cfl_for, 2, quantile, level / 100)

  # Computing x and y limits of plot
  plot_ylim <- range(exp_cfl_hist, exp_cfl_mean, exp_cfl_lower, exp_cfl_upper, na.rm = TRUE)
  plot_xlim <- c(years_hist[1], tail(years_for, 1))

  # Initial plot of historical values
  plot(x = years_hist,
       y = exp_cfl_hist,
       xlim = plot_xlim,
       ylim = plot_ylim,
       type = "l",
       xlab = "Years",
       ylab = "Expected Curtate Future Lifetime (Years)")

  # Preparing fanplot parameters
  fan_col <- colorRampPalette(c("grey60", rgb(1, 1, 1)))
  fan_n <- 1

  # Adding confidence intervals
  fanplot::fan(rbind(exp_cfl_lower, exp_cfl_upper),
               data.type = "values",
               start = years_for[1],
               anchor = tail(exp_cfl_hist, 1),
               probs = c(level / 200, 1 - level / 200),
               fan.col = fan_col, n.fan = fan_n + 1, ln = NULL)

  # Adding mean
  lines(x = c(tail(years_hist, 1), years_for),
        y = c(tail(exp_cfl_hist, 1), exp_cfl_mean))

}


