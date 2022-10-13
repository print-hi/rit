#' Coale and Kisker Method
#'
#' Implements the Coale and Kisker method of mortality rate completion for old
#' ages.
#'
#' @inheritParams complete_old_age
#' @param m_end
#' constant or vector specifying the central death rates at the final age for
#' each calendar year. If supplied as a vector, vector length and number of
#' columns for `rates` must be equal
#'
#' @return
#' completed mortality rates for all ages and old ages in the same format as
#' `rates`
#'
#' @export
#'
coale_kisker <- function(rates, ages, old_ages, type = "central", closure_age = 130, m_end = 1, years = NULL) {

# Flagging Errors ---------------------------------------------------------

  # rates
  if (!is.vector(rates) & !is.matrix(rates) & !(is.array(rates) & length(dim(rates)) == 3)) {
    stop("rates must be a vector, 2D matrix or a 3D array")
  }

  if (!is.numeric(rates)) {
    stop("rates must be numeric")
  }

  if (any(rates < 0, na.rm = T)) {
    stop("rates must be non-negative")
  }

  if (type == "prob" & any(rates > 1, na.rm = T)) {
    stop("1-yr death probabilities must be less than or equal to 1")
  }

  # ages
  if (length(ages) != NROW(rates)) {
    stop("length of ages must be equal to number of rows of rates")
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

  # old_ages
  if (!is.vector(old_ages) | !all(old_ages == floor(old_ages))) {
    stop("old ages must be a vector of integers")
  }

  if (is.unsorted(old_ages) | utils::tail(old_ages, 1) - old_ages[1] + 1 != length(old_ages)) {
    stop("old ages must be increasing by 1 at each step")
  }

  if (any(old_ages < 0)) {
    stop("old ages must be non-negative")
  }
  # note, ages <- 55:110 and old_ages <- 90:130 is valid
  if (old_ages[1] -  utils::tail(ages, 1) > 1 | old_ages[1] <= ages[1]) {
    stop("old ages must connect with ages")
  }

  # type
  if (!is.element(type, c("central", "prob", "force"))) {
    stop("type must be 'central', 'prob' or 'force'")
  }

  # closure_age

  if (closure_age != utils::tail(old_ages, 1)) {
    stop("closure age must be the last age in old ages")
  }

  # m_end
  if (!is.numeric(m_end) | !is.vector(m_end)) {
    stop("closure central death rate must be a numeric vector")
  }

  if (length(m_end) != 1 & length(m_end) != NCOL(rates)) {
    stop("length of closure central death rate must be 1 or equal to number of
         columns of rates")
  }

  # years
  if (!is.null(years)) {
    if (length(years) != NCOL(rates)) {
      stop("length of years must be equal to number of columns of rates")
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



  # Convert to central death rates
  mxy_arr <- rate2rate(rates, from = type, to = "central")

  # Obtaining relevant ages
  boundary_age <- old_ages[1] - 1
  kept_ages <- ages[1]:boundary_age
  n <- closure_age - old_ages[1] + 1

  # CK for 2D matrix
  CK_mat <- function(mxy_mat) {

    # Convert vector to matrix if necessary
    mxy <- as.matrix(mxy_mat)

    # Calculating parameter sy
    m_boundary <- mxy[boundary_age - ages[1] + 1, ]
    k_boundary <- log(m_boundary/mxy[boundary_age - ages[1], ])
    sy <- 2 / (n * (n + 1)) * (n * k_boundary - log(m_end / m_boundary))

    old_kxy <- outer(rep(1, length(old_ages)), k_boundary) - outer(old_ages - boundary_age, sy)
    # Outer creates matrix of appropriate dimensions for element-wise matrix multiplication

    # Completion of old ages
    old_kxy_cumsum <- apply(old_kxy, 2, cumsum)
    old_mxy <- outer(rep(1, length(old_ages)), m_boundary) * exp(old_kxy_cumsum)

    # Preparing final output
    kept_mxy <- mxy[1:(boundary_age - ages[1] + 1), , drop = F]
    completed_mxy <- rbind(kept_mxy, old_mxy)

    completed_mxy
  }

  # Ensure output is same format as input
  if (is.vector(mxy_arr)) {
    completed_mxy_vec <- as.vector(CK_mat(mxy_arr))
    names(completed_mxy_vec) <- as.character(c(kept_ages, old_ages))

    # Convert to required mortality rate
    return(rate2rate(completed_mxy_vec, from = "central", to = type))

  } else if (is.matrix(mxy_arr)) {
    completed_mxy_arr <- CK_mat(mxy_arr)
  } else if (is.array(mxy_arr)) {
    completed_mxy_arr <- arr_apply(mxy_arr, CK_mat)
  }

  rownames(completed_mxy_arr) <- as.character(c(kept_ages, old_ages))
  colnames(completed_mxy_arr) <- if (is.null(years)) colnames(rates) else as.character(years)

  # Convert to required mortality rate
  return(rate2rate(completed_mxy_arr, from = "central", to = type))

}

#' Denuit and Goderniaux Method
#'
#' Implements the Denuit and Goderniaux method of mortality rate completion for
#' old ages.
#'
#' @inheritParams complete_old_age
#' @param start_fit_age
#' model is fitted to ages starting from this age
#' @param smoothing
#' logical value indicating if smoothing is to be applied
#'
#' @return
#' completed mortality rates for all ages and old ages in the same format as
#' `rates`
#'
#' @export
#'
denuit_goderniaux <- function(rates, ages, old_ages, type = "prob", closure_age = 130, start_fit_age = 75, smoothing = FALSE, years = NULL) {


# Flagging Errors ---------------------------------------------------------

  # rates
  if (!is.vector(rates) & !is.matrix(rates) & !(is.array(rates) & length(dim(rates)) == 3)) {
    stop("rates must be a vector, 2D matrix or a 3D array")
  }

  if (!is.numeric(rates)) {
    stop("rates must be numeric")
  }

  if (any(rates < 0, na.rm = T)) {
    stop("rates must be non-negative")
  }

  if (type == "prob" & any(rates > 1, na.rm = T)) {
    stop("1-yr death probabilities must be less than or equal to 1")
  }

  # ages
  if (length(ages) != NROW(rates)) {
    stop("length of ages must be equal to number of rows of rates")
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

  # old_ages
  if (!is.vector(old_ages) | !all(old_ages == floor(old_ages))) {
    stop("old ages must be a vector of integers")
  }

  if (is.unsorted(old_ages) | utils::tail(old_ages, 1) - old_ages[1] + 1 != length(old_ages)) {
    stop("old ages must be increasing by 1 at each step")
  }

  if (any(old_ages < 0)) {
    stop("old ages must be non-negative")
  }
  # note, ages <- 55:110 and old_ages <- 90:130 is valid
  if (old_ages[1] -  utils::tail(ages, 1) > 1 | old_ages[1] <= ages[1]) {
    stop("old ages must connect with ages")
  }

  # type
  if (!is.element(type, c("central", "prob", "force"))) {
    stop("type must be 'central', 'prob' or 'force'")
  }

  # closure_age

  if (closure_age != utils::tail(old_ages, 1)) {
    stop("closure age must be the last age in old ages")
  }

  # start_fit_age
  if (start_fit_age != floor(start_fit_age) | !is.element(start_fit_age, ages)) {
    stop("start fit age must be an integer in ages")
  }

  # smoothing
  if (!is.logical(smoothing)) {
    stop("smoothing must be TRUE or FALSE")
  }

  # years
  if (!is.null(years)) {
    if (length(years) != NCOL(rates)) {
      stop("length of years must be equal to number of columns of rates")
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

  # Convert to death probabilities
  qxy_arr <- rate2rate(rates, from = type, to = "prob")

  # Obtaining relevant ages
  boundary_age <- old_ages[1] - 1
  kept_ages <- ages[1]:boundary_age
  fitted_ages <- start_fit_age:ages[length(ages)]

  # DG for 2D matrix
  DG_mat <- function(qxy_mat) {

    # Convert vector to matrix if necessary
    qxy <- as.matrix(qxy_mat)

    # Creating data frame to fit log-quadratic model
    input_df <- as.data.frame(qxy)
    df_fit <- input_df[ages >= start_fit_age, , drop = F]

    # Helper function to fit log-quadratic model on a vector
    DG_fit <- function(qx) {

      log_quad_fit <- stats::lm(log(qx) ~ I((closure_age - fitted_ages)^2) - 1)
      stats::coefficients(log_quad_fit)
    }

    # Calculating regression coefficient c
    cy <- as.numeric(lapply(df_fit, DG_fit))
    cxy <- outer(rep(1, length(old_ages)), cy) # Outer creates matrix of
    # appropriate dimensions for element-wise matrix multiplication

    # Completion for old ages
    old_qxy <- exp(cxy * (closure_age - old_ages)^2)

    # Preparing final output
    kept_qxy <- qxy[1:(boundary_age - ages[1] + 1), , drop = F]
    completed_qxy <- rbind(kept_qxy, old_qxy)

    if (smoothing) {

      smooth_ages <- (boundary_age - 4):(boundary_age + 5)
      smooth_indices <- smooth_ages - ages[1] + 1

      smooth_rate <- function(index) {
        rough_mat <- completed_qxy[(index - 2):(index + 2), , drop = F]
        apply(rough_mat, 2, function(y) exp(mean(log(y))))
      }

      smooth_mat <- do.call(rbind, lapply(smooth_indices, smooth_rate))
      completed_qxy[smooth_indices, ] <- smooth_mat

    }

    completed_qxy

  }

  # Ensure output is same format as input
  if (is.vector(qxy_arr)) {
    completed_qxy_vec <- as.vector(DG_mat(qxy_arr))
    names(completed_qxy_vec) <- as.character(c(kept_ages, old_ages))

    # Convert to required mortality rate
    return(rate2rate(completed_qxy_vec, from = "prob", to = type))

  } else if (is.matrix(qxy_arr)) {
    completed_qxy_arr <- DG_mat(qxy_arr)
  } else if (is.array(qxy_arr)) {
    completed_qxy_arr <- arr_apply(qxy_arr, DG_mat)
  }

  rownames(completed_qxy_arr) <- as.character(c(kept_ages, old_ages))
  colnames(completed_qxy_arr) <- if (is.null(years)) colnames(rates) else as.character(years)

  # Convert to required mortality rate
  return(rate2rate(completed_qxy_arr, from = "prob", to = type))

}

#' Kannisto Method of Age Completion
#'
#' Implements the Kannisto method of age completion for old ages.
#'
#' @inheritParams complete_old_age
#' @param fitted_ages
#' vector of ages for which model is fitted on
#'
#' @return
#' completed mortality rates for all ages and old ages in the same format as
#' `rates`
#'
#' @export
#'
kannisto <- function(rates, ages, old_ages, fitted_ages, type = "force", closure_age = 130, years = NULL) {


# Flagging Errors ---------------------------------------------------------
  # rates
  if (!is.vector(rates) & !is.matrix(rates) & !(is.array(rates) & length(dim(rates)) == 3)) {
    stop("rates must be a vector, 2D matrix or a 3D array")
  }

  if (!is.numeric(rates)) {
    stop("rates must be numeric")
  }

  if (any(rates < 0, na.rm = T)) {
    stop("rates must be non-negative")
  }

  if (type == "prob" & any(rates > 1, na.rm = T)) {
    stop("1-yr death probabilities must be less than or equal to 1")
  }

  # ages
  if (length(ages) != NROW(rates)) {
    stop("length of ages must be equal to number of rows of rates")
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

  # old_ages
  if (!is.vector(old_ages) | !all(old_ages == floor(old_ages))) {
    stop("old ages must be a vector of integers")
  }

  if (is.unsorted(old_ages) | utils::tail(old_ages, 1) - old_ages[1] + 1 != length(old_ages)) {
    stop("old ages must be increasing by 1 at each step")
  }

  if (any(old_ages < 0)) {
    stop("old ages must be non-negative")
  }
  # note, ages <- 55:110 and old_ages <- 90:130 is valid
  if (old_ages[1] -  utils::tail(ages, 1) > 1 | old_ages[1] <= ages[1]) {
    stop("old ages must connect with ages")
  }

  # fitted_ages
  if (!is.vector(fitted_ages) | !all(fitted_ages == floor(fitted_ages))) {
    stop("fitted ages must be a vector of integers")
  }

  if (is.unsorted(fitted_ages) | utils::tail(fitted_ages, 1) - fitted_ages[1] + 1 != length(fitted_ages)) {
    stop("fitted ages must be increasing by 1 at each step")
  }

  if (any(fitted_ages < 0)) {
    stop("fitted ages must be non-negative")
  }
  # e.g., ages <- 55:110, old_ages <- 90:130, fitted_ages <- 75:89 is valid
  if (utils::tail(fitted_ages, 1) + 1 != old_ages[1]) {
    stop("fitted ages must connect with old ages")
  }

  # type
  if (!is.element(type, c("central", "prob", "force"))) {
    stop("type must be 'central', 'prob' or 'force'")
  }

  # closure_age

  if (closure_age != utils::tail(old_ages, 1)) {
    stop("closure age must be the last age in old ages")
  }

  # years
  if (!is.null(years)) {
    if (length(years) != NCOL(rates)) {
      stop("length of years must be equal to number of columns of rates")
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

  # Convert to force of mortality
  muxy_arr <- rate2rate(rates, from = type, to = "force")

  # Obtaining relevant ages
  kept_ages <- ages[1]:(old_ages[1] - 1)

  # Kannisto for 2D matrix
  kannisto_mat <- function(muxy_mat) {

    # Convert vector to matrix if necessary
    muxy <- as.matrix(muxy_mat)

    # Creating data frame to fit model
    input_df <- as.data.frame(muxy)
    df_fit <- input_df[fitted_ages - ages[1] + 1, , drop = F]

    # Defining helper functions to fit model and extrapolate
    logit <- function(x) log(x / (1 - x))
    logistic <- function(x) exp(x) / (1 + exp(x))

    kannisto_fit <- function(mux) {
      fit <- stats::lm(logit(mux) ~ fitted_ages)
      stats::coefficients(fit)
    }

    # Calculating regression coefficients
    fit_coeff <- sapply(df_fit, kannisto_fit)
    ay <- exp(fit_coeff[1,])
    by <- fit_coeff[2,]
    axy <- outer(rep(1, length(old_ages)), ay)
    bxy <- outer(rep(1, length(old_ages)), by) # Outer creates matrix of
    # appropriate dimensions for element-wise matrix multiplication

    old_ages_mat <- outer(old_ages, rep(1, dim(muxy)[2]))
    old_muxy <- logistic(log(axy) + bxy*old_ages_mat) # Convert vector
    # to matrix if necessary

    # Preparing final output
    kept_muxy <- muxy[kept_ages - ages[1] + 1, , drop = F]
    completed_muxy <- rbind(kept_muxy, old_muxy)


    # Enforce closure age constraint
    if (is.element(closure_age, old_ages)) {
      completed_muxy[closure_age - ages[1] + 1, ] <- 30
    }

    completed_muxy

  }

  if (is.vector(muxy_arr)) {
    completed_muxy_vec <- as.vector(kannisto_mat(muxy_arr))
    names(completed_muxy_vec) <- as.character(c(kept_ages, old_ages))

    # Convert to required mortality rate
    return(rate2rate(completed_muxy_vec, from = "force", to = type))
  } else if (is.matrix(muxy_arr)) {
    completed_muxy_arr <- kannisto_mat(muxy_arr)
  } else if (is.array(muxy_arr)) {
    completed_muxy_arr <- arr_apply(muxy_arr, kannisto_mat)
  }

  rownames(completed_muxy_arr) <- as.character(c(kept_ages, old_ages))
  colnames(completed_muxy_arr) <- if (is.null(years)) colnames(rates) else as.character(years)

  # Convert to required mortality rate
  return(rate2rate(completed_muxy_arr, from = "force", to = type))

}

#' Mortality Rate Completion
#'
#' Completes mortality rates at old ages.
#'
#' @param rates
#' vector, matrix or 3D array of mortality rates with age (on the rows) and
#' calendar year or cohort (on the columns) and simulation number (3rd dimension)
#' @param ages
#' vector of ages for `rates`
#' @param old_ages
#' vector of old ages for which `rates` is to be completed for
#' @param method
#' character string representing the completion method to be used. Takes the
#' following values: "CK" for Coale-Kisker, "DG" for Denuit and Goderniaux,
#' "kannisto" for Kannisto
#' @param type
#' character string representing the type of mortality rate for input and output.
#' Takes the following values: "central" for central death rates,
#' "prob" for 1-year death probabilities, "force" for force of mortality
#' @param closure_age
#' maximum life span
#' @param years
#' optional vector of years for `rates`. If not supplied, then the column names
#' of `rates` will be preserved
#' @param ...
#' additional arguments for the chosen completion method. See
#' \code{\link{coale_kisker}}, \code{\link{denuit_goderniaux}}, \code{\link{kannisto}}
#'
#' @return
#' completed mortality rates for all ages and old ages in the same format as
#' `rates`
#' @export
#'
complete_old_age <- function(rates, ages, old_ages, method = "kannisto",
                             type = "prob", closure_age = 130, years = NULL, ...) {

  # error flagging already done in individual functions

  valid_methods = c("CK", "DG", "kannisto")
  if (!is.element(method, valid_methods)) stop("invalid completion method")

  if (method == "CK") {
    coale_kisker(rates = rates, ages = ages, old_ages = old_ages,
                 type = type, closure_age = closure_age, years = years, ...)
  } else if (method == "DG") {
    denuit_goderniaux(rates = rates, ages = ages, old_ages = old_ages,
                 type = type, closure_age = closure_age, years = years, ...)
  } else if (method == "kannisto") {
    kannisto(rates = rates, ages = ages, old_ages = old_ages,
             type = type, closure_age = closure_age, years = years, ...)
  }
}
