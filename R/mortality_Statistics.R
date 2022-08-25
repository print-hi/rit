#' The Survival Function
#'
#' Distribution and quantile function  of the survival function.
#'
#' Linear interpolation is performed between the discrete points of the survival function
#'
#' @param surv_fun
#' vector of survival function
#' @param surv_time
#' vector of survival times
#' @param surv_prob
#' vector of survival probabilities
#'
#' @name surv
NULL

#' @rdname surv
#'
#' @export
psurv <- function(surv_fun, surv_time) {

# Flagging Errors ---------------------------------------------------------

  # surv_fun
  if (!is.vector(surv_fun) | !is.numeric(surv_fun)) {
    stop("survival function must be a numeric vector")
  }

  if (any(surv_fun < 0, na.rm = T)) {
    stop("survival function must be non-negative")
  }

  if (any(surv_fun > 1, na.rm = T)) {
    stop("survival function must be less than or equal to 1")
  }

  # surv_time
  if (!is.vector(surv_time) | !is.numeric(surv_time)) {
    stop("survival time must be a numeric vector")
  }


# Implementation ----------------------------------------------------------

  n <- length(surv_fun)

  # Checking bounds
  stopifnot(surv_time >= 0)

  # Linear interpolation
  surv_fun_approx <- stats::approxfun(0:(n - 1), surv_fun)

  result <- surv_fun_approx(surv_time)
  # Note: returns NA for surv_time >= n - 1

  return(result)

}

#' @rdname surv
#'
#' @export
qsurv <- function(surv_fun, surv_prob) {

# Flagging Errors ---------------------------------------------------------

  # surv_fun
  if (!is.vector(surv_fun) | !is.numeric(surv_fun)) {
    stop("survival function must be a numeric vector")
  }

  if (any(surv_fun < 0, na.rm = T)) {
    stop("survival function must be non-negative")
  }

  if (any(surv_fun > 1, na.rm = T)) {
    stop("survival function must be less than or equal to 1")
  }

  # surv_prob

  if (!is.vector(surv_prob) | !is.numeric(surv_prob)) {
    stop("survival probabilities must be a numeric vector")
  }

  if (any(surv_prob < 0, na.rm = T)) {
    stop("survival probabilities must be non-negative")
  }

  if (any(surv_prob > 1, na.rm = T)) {
    stop("survival probabilities must be less than or equal to 1")
  }

  # surv_time


# Implementation ----------------------------------------------------------


  n <- length(surv_fun)

  # Checking bounds
  stopifnot(surv_prob >= 0, surv_prob <= 1)

  # Creating list of linearly interpolated functions shifted by element of surv_prob
  surv_fun_list <- lapply(surv_prob, function(p) stats::approxfun(0:(n - 1), surv_fun - p))

  # Use uniroot to determine quantile
  sapply(surv_fun_list, function(fn) stats::uniroot(fn, c(0, (n - 1)))$root)

}


#' Summarise Curtate Future Lifetime Statistics
#'
#' Produces expected curtate future lifetime for a 3D array of 1-year
#' death probabilities.
#'
#' @param qx
#' vector, matrix or 3D array of 1-year death probabilities with age
#' (on the rows) and calendar year or cohort (on the columns) and simulation
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
#' expected curtate future lifetime as a matrix if `qx` is a 3D array
#' with simulation number (on the rows) and calendar year or cohort (on the columns).
#' Returns a vector otherwise
#'
#' @export
#'
exp_cfl <- function(qx, ages, init_age = NULL, years = NULL) {

# Flagging Errors ---------------------------------------------------------

  # qx
  if (!is.vector(qx) & !is.matrix(qx) & !(is.array(qx) & length(dim(qx)) == 3)) {
    stop("qx must be a vector, 2D matrix or a 3D array")
  }

  if (!is.numeric(qx)) {
    stop("qx must be numeric")
  }

  if (any(qx < 0, na.rm = T)) {
    stop("qx must be non-negative")
  }

  if (any(qx > 1, na.rm = T)) {
    stop("qx must be less than or equal to 1")
  }

  # ages
  if (length(ages) != NROW(qx)) {
    stop("length of ages must be equal to number of rows of qx")
  }

  if (!is.vector(ages) | !all(ages == floor(ages))) {
    stop("ages must be a vector of integers")
  }

  if (is.unsorted(ages) | utils::tail(ages, 1) - ages[1] + 1 != length(ages)) {
    stop("ages must be increasing by 1 at each step")
  }

  if (any(ages < 0)) {
    stop("ages must be non-negative")
  }

  # years
  if (!is.null(years)) {
    if (length(years) != NCOL(qx)) {
      stop("length of years must be equal to number of columns of qx")
    }

    if (!is.vector(years) | !all(years == floor(years))) {
      stop("years must be a vector of integers")
    }

    if (is.unsorted(years) | utils::tail(years, 1) - years[1] + 1 != length(years)) {
      stop("years must be increasing by 1 at each step")
    }

    if (any(years < 0)) {
      stop("years must be non-negative")
    }
  }

# Implementation ----------------------------------------------------------

  # Converting to 1-year survival probabilities
  if(is.null(init_age)) {
    init_age <- ages[1]
  } else if (!is.element(init_age, ages)) {
    stop("invalid initial age")
  }

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
    result <- exp_cfl_mat(kpx)

    return(result)
  } else {
    result <- arr_apply(kpx, exp_cfl_mat)
    rownames(result) <- if (is.null(years)) colnames(qx) else as.character(years)

    return(t(result))
  }

}


#' Plot Curtate Future Lifetime Simulations
#'
#' Plots simulated expected curtate future lifetime.
#'
#' @param exp_cfl_rates
#' matrix of simulated expected curtate future lifetime with simulation number
#' (on the rows) and calendar year or cohort (on the columns).
#' Should be generated from \code{\link{exp_cfl}}
#' @param years
#' vector of years for \code{exp_cfl_rates}
#' @param level
#' desired confidence level with 95% as default
#'
#' @export
#'
plot_exp_cfl <- function(exp_cfl_rates, years, level = 95) {


# Flagging Errors ---------------------------------------------------------


  # exp_cfl_rates
  if (!is.numeric(exp_cfl_rates) | !is.matrix(exp_cfl_rates)) {
    stop("exp_cfl_rates must be a numeric matrix")
  }

  # years
  if (length(years) != NCOL(exp_cfl_rates)) {
    stop("length of years must be equal to number of columns of exp_cfl_rates")
  }

  if (!is.vector(years) | !all(years == floor(years))) {
    stop("years must be a vector of integers")
  }

  if (is.unsorted(years) | utils::tail(years, 1) - years[1] + 1 != length(years)) {
    stop("years must be increasing by 1 at each step")
  }

  if (any(years < 0)) {
    stop("years must be non-negative")
  }

  # level
  if (level > 100 | level < 0) {
    stop("level must be between 0 and 100")
  }


# Implementation ----------------------------------------------------------

  # Calculating mean, upper and lower values of confidence interval for simulations
  # of expected curtate future lifetime
  exp_cfl_mean <- apply(exp_cfl_rates, 2, mean)
  exp_cfl_lower <- apply(exp_cfl_rates, 2, stats::quantile, 1 / 2 - level / 200)
  exp_cfl_upper <- apply(exp_cfl_rates, 2, stats::quantile, 1 / 2 + level / 200)

  # Computing x and y limits of plot
  plot_ylim <- range(exp_cfl_mean, exp_cfl_lower, exp_cfl_upper, na.rm = TRUE)
  plot_xlim <- range(years)

  # Initial plot of mean
  plot(x = years,
       y = exp_cfl_mean,
       xlim = plot_xlim,
       ylim = plot_ylim,
       type = "l",
       xlab = "Years",
       ylab = "Expected Curtate Future Lifetime (Years)")

  # Preparing fanplot parameters
  fan_col <- grDevices::colorRampPalette(c("grey60", grDevices::rgb(1, 1, 1)))
  fan_n <- 1

  # Adding confidence intervals
  fanplot::fan(rbind(exp_cfl_lower, exp_cfl_upper),
               data.type = "values",
               start = years[1],
               probs = c(1 / 2 - level / 200, 1 / 2 + level / 200),
               fan.col = fan_col, n.fan = fan_n + 1, ln = NULL)

  # Overlaying mean on top of confidence intervals
  graphics::lines(x = years, y = exp_cfl_mean)

}

#' Plot Survival Function Simulations
#'
#' Plots simulated survival functions for a desired cohort.
#'
#' @param surv_sim
#' 3D array of forecasted survival functions with survival time
#' (on the rows) and calendar year or cohort (on the columns) and simulation
#' number (3rd dimension)
#' @param init_age
#' initial age for which `surv_sim` was calculated at
#' @param target_year
#' year for which the survival function is plotted for
#' @param level
#' desired confidence level with 95% as default
#' @param years
#' optional vector of years for `surv_sim`. If not supplied, then the column names
#' of `surv_sim` will be preserved
#'
#' @export
#'
plot_surv_sim <- function(surv_sim, init_age, target_year, level = 95, years = NULL) {

# Flagging Errors ---------------------------------------------------------

  # surv_sim
  if (!(is.array(surv_sim) & length(dim(surv_sim)) == 3)) {
    stop("simulated survival functions must be a 3D array")
  }

  if (!is.numeric(surv_sim)) {
    stop("survival function must be numeric")
  }

  if (any(surv_sim < 0, na.rm = T)) {
    stop("survival function must be non-negative")
  }

  if (any(surv_sim > 1, na.rm = T)) {
    stop("survival function must be less than or equal to 1")
  }

  # init_age
  if (init_age != floor(init_age)) {
    stop("initial age must be an integer")
  }

  # target_year
  if (!is.element(target_year, as.numeric(colnames(surv_sim)))) {
    stop("target year must be included in years for simulations")
  }

  if (!is.null(years) & !is.element(target_year, years)) {
    stop("target year must be included in years for simulations")
  }

  # level
  if (level > 100 | level < 0) {
    stop("level must be between 0 and 100")
  }

  # years
  if (!is.null(years)) {
    if (length(years) != NCOL(surv_sim)) {
      stop("length of years must be equal to number of columns of the simulated survival functions")
    }

    if (!is.vector(years) | !all(years == floor(years))) {
      stop("years must be a vector of integers")
    }

    if (is.unsorted(years) | utils::tail(years, 1) - years[1] + 1 != length(years)) {
      stop("years must be increasing by 1 at each step")
    }

    if (any(years < 0)) {
      stop("years must be non-negative")
    }
  }



# Implementation ----------------------------------------------------------

  # Extracting required variables from inputs
  surv_time <- as.numeric(rownames(surv_sim))
  surv_sim_cohort <- t(surv_sim[, as.character(target_year), ])

  # Calculating mean, upper and lower values of confidence interval for simulations
  # of survival function
  surv_mean <- apply(surv_sim_cohort, 2, mean)
  surv_lower <- apply(surv_sim_cohort, 2, stats::quantile, 1 / 2 - level / 200)
  surv_upper <- apply(surv_sim_cohort, 2, stats::quantile, 1 / 2 + level / 200)

  # Computing x and y limits of plot
  plot_ylim <- range(surv_mean, surv_lower, surv_upper, na.rm = TRUE)
  plot_xlim <- c(surv_time[1], utils::tail(surv_time, 1))

  # Generating new plot
  plot(NULL,
       xlim = plot_xlim,
       ylim = plot_ylim,
       xlab = "Survival Time (Years)",
       ylab = "Survival Probability",
       main = paste("Age", init_age, "in Year", target_year))

  # Preparing fanplot parameters
  fan_col <- grDevices::colorRampPalette(c("grey60", grDevices::rgb(1, 1, 1)))
  fan_n <- 1

  # Adding confidence intervals
  fanplot::fan(rbind(surv_lower, surv_upper),
               data.type = "values",
               start = surv_time[1],
               probs = c(1 / 2 - level / 200, 1 / 2 + level / 200),
               fan.col = fan_col, n.fan = fan_n + 1, ln = NULL)

  # Plot mean
  graphics::lines(x = surv_time, y = surv_mean, type = 'l')

}
