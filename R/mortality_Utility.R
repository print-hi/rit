#' Apply Functions Over Each Matrix of 3D Array
#'
#' Returns a 3D array of matrices obtained by applying a function over each matrix
#' of a 3D array.
#'
#' @param X
#' 3D array
#' @param FUN
#' the function to be applied
#'
#' @return
#' 3D array of matrices obtained by applying a function
#' @keywords internal
arr_apply <- function(X, FUN) {

  X_list <- lapply(seq(dim(X)[3]), function(i) matrix(X[, , i], nrow = dim(X)[1], ncol = dim(X)[2]))
  sapply(X_list, FUN, simplify = "array")
}

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



#' Convert from Period to Cohort Rates
#'
#' Converts from period to cohort mortality rates
#'
#' @param period_rates
#' matrix or 3D array of mortality rates with age (on the rows) and calendar
#' year (on the columns) and simulation number (3rd dimension)
#' @param ages
#' vector of ages for `period_rates`
#' @param init_age
#' initial age for which the cohort is to be considered. If not provided,
#' the cohort will correspond to the smallest age supplied in `ages`
#'
#' @return
#' associated cohort mortality rates as a 3D array if `period_rates` is an array,
#' or as a matrix otherwise
#' @export
#'
period2cohort <- function(period_rates, ages, init_age = NULL) {


# Flagging errors ---------------------------------------------------------

  # period_rates

  if (!is.matrix(period_rates) & !(is.array(period_rates) & length(dim(period_rates)) == 3)) {
    stop("period rates must be a 2D matrix or a 3D array")
  }

  if (!is.numeric(period_rates)) {
    stop("period rates must be numeric")
  }

  if (any(period_rates < 0, na.rm = T)) {
    stop("period rates must be non-negative")
  }


  # ages
  if (length(ages) != NROW(period_rates)) {
    stop("length of ages must be equal to number of rows of period rates")
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


# Implementation ----------------------------------------------------------

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


  if (is.matrix(period_rates)) {
    cohort_rates <- p2c_mat(period_rates)
  } else if (is.array(period_rates)) {
    cohort_rates <- arr_apply(period_rates, p2c_mat)
  }

  dimnames(cohort_rates) <- dimnames(period_rates)

  cohort_rates

}


#' Convert from Cohort to Period Rates
#'
#' Converts from cohort to period mortality rates
#'
#' @param cohort_rates
#' matrix or 3D array of mortality rates with age (on the rows) and cohort
#' (on the columns) and simulation number (3rd dimension)
#'
#' @return
#' associated period mortality rates as a 3D array if `cohort_rates` is an array,
#' or as a matrix otherwise
#' @export
#'
cohort2period <- function(cohort_rates) {

# Flagging errors ---------------------------------------------------------

  # cohort_rates

  if (!is.matrix(cohort_rates) & !(is.array(cohort_rates) & length(dim(cohort_rates)) == 3)) {
    stop("cohort rates must be a 2D matrix or a 3D array")
  }

  if (!is.numeric(cohort_rates)) {
    stop("cohort rates must be numeric")
  }

  if (any(cohort_rates < 0, na.rm = T)) {
    stop("cohort rates must be non-negative")
  }


# Implementation ----------------------------------------------------------

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


#' Generate Default Death Probabilities
#'
#' Generates default death probabilities to be used for simulating paths if
#' necessary.
#'
#' @param init_age
#' the initial age of the path
#' @param sex
#' character string denoting the gender of individuals, "F" for female and "M" for male
#' @param closure_age
#' maximum life span
#'
#' @return
#' a vector of 1-year death probabilities corresponding to the specified
#' initial and closure age
#'
#' @keywords internal
generate_default_qx <- function(init_age, sex = "F", closure_age = 130) {

# Flagging errors ---------------------------------------------------------

    # init_age
    if (init_age < 55 | init_age > closure_age) {
        stop("initial age must be between 55 and the maximum age")
    }

    if (init_age != floor(init_age)) {
        stop("initial age must be an integer")
    }

    # sex
    if (sex != "F" & sex != "M") {
        stop("sex must be 'F' or 'M'")
    }

    # closure_age
    if (closure_age < 90 | closure_age != floor(closure_age)) {
        stop("maximum age must be an integer greater than 89")
    }


# Implementation ----------------------------------------------------------

    # Generating default death probabilities
    young_ages <- 55:89

    if (sex == "F") {
        AUS_StMoMo <- StMoMo::StMoMoData(rit::mortality_AUS_data, series = "female")
        rates_hist <- rit::mortality_AUS_data$rate$female[as.character(young_ages), ]
    } else {
        AUS_StMoMo <- StMoMo::StMoMoData(rit::mortality_AUS_data, series = "male")
        rates_hist <- rit::mortality_AUS_data$rate$male[as.character(young_ages), ]
    }

    # Using M7 model to Forecast Rates
    M7 <- StMoMo::m7()
    AUS_Ini_Data <- StMoMo::central2initial(AUS_StMoMo)
    ages_fit <- young_ages
    wxy <- StMoMo::genWeightMat(ages = ages_fit, years = AUS_Ini_Data$years, clip = 3)
    M7_fit <- StMoMo::fit(M7, data = AUS_Ini_Data, ages.fit = ages_fit, wxt = wxy)
    M7_for <- forecast::forecast(M7_fit, h = 100)

    # Mortality Rate Completion with Kannisto Method
    old_ages <- 90:closure_age
    ages <- c(young_ages, old_ages)
    kannisto_hist <- complete_old_age(rates = rates_hist, ages = young_ages,
                                      old_ages = old_ages, fitted_ages = 80:89,
                                      closure_age = closure_age,
                                      method = "kannisto", type = "central")
    kannisto_for <- complete_old_age(rates = M7_for$rates, ages = young_ages,
                                     old_ages = old_ages, fitted_ages = 80:89,
                                     closure_age = closure_age,
                                     method = "kannisto", type = "central")

    # Combine Historical and Forecasted Rates
    kannisto_55_period <- cbind(kannisto_hist, kannisto_for)
    qx_period <- rate2rate(kannisto_55_period, from = "central", to = "prob")
    # Take year 2022
    death_probs <- period2cohort(qx_period, ages = ages, init_age = init_age)[, "2022"]

    return(death_probs)
}

