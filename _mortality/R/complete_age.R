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
#'
#' @examples
#'
arr_apply <- function(X, FUN) {

  X_list <- lapply(seq(dim(X)[3]), function(i) matrix(X[, , i], nrow = dim(X)[1], ncol = dim(X)[2]))
  sapply(X_list, FUN, simplify = "array")
}

#' Coale and Kisker Method
#'
#' Implements the Coale and Kisker method of mortality rate completion for old
#' ages.
#'
#' @param rates
#' matrix or vector of mortality rates with age (on the rows) and calendar year
#' (on the columns). Vector is equivalent to a matrix with a single column
#' @param ages
#' vector of ages for `rates`
#' @param old_ages
#' vector of old ages for which `rates` is to be completed for
#' @param type
#' character string representing the type of mortality rate supplied. Takes the
#' following values: "central" for central death rates, "prob" for 1-year death
#' probabilities, "force" for force of mortality
#' @param closure_age
#' maximum life span
#' @param m_end
#' constant or vector specifying the central death rates at the final age for
#' each calendar year. If supplied as a vector, vector length and number of
#' columns for `rates` must be equal
#' @param years
#' optional vector of years for `rates`. If not supplied, then the column names
#' of `rates` will be preserved
#'
#' @return
#' matrix of central death rates for all ages and calendar years
#'
#' @export
#'
#' @examples
#'
CK <- function(rates, ages, old_ages, type = "central", closure_age = 130, m_end = 1, years = NULL) {

  if (is.null(years)) {
    col_names <- colnames(rates)
  }

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

  if (is.vector(mxy_arr) | is.matrix(mxy_arr)) {
    completed_mxy_arr <- CK_mat(mxy_arr)
  } else {
    completed_mxy_arr <- arr_apply(mxy_arr, CK_mat)
  }

  rownames(completed_mxy_arr) <- as.character(c(kept_ages, old_ages))
  colnames(completed_mxy_arr) <- if (is.null(years)) col_names else as.character(years)

  completed_mxy_arr

}

#' Denuit and Goderniaux Method
#'
#' Implements the Denuit and Goderniaux method of mortality rate completion for
#' old ages.
#'
#' @param rates
#' matrix or vector of mortality rates with age (on the rows) and calendar year
#' (on the columns). Vector is equivalent to a matrix with a single column
#' @param ages
#' vector of ages for `rates`
#' @param old_ages
#' vector of old ages for which `rates` is to be completed for
#' @param type
#' character string representing the type of mortality rate supplied. Takes the
#' following values: "central" for central death rates, "prob" for 1-year death
#' probabilities, "force" for force of mortality
#' @param closure_age
#' maximum life span
#' @param start_fit_age
#' model is fitted to ages starting from this age
#' @param smoothing
#' logical value indicating if smoothing is to be applied
#' @param years
#' optional vector of years for `rates`. If not supplied, then the column names
#' of `rates` will be preserved
#'
#' @return
#' matrix of 1-year death probabilities for all ages and calendar years
#'
#' @export
#'
#' @examples
#'
DG <- function(rates, ages, old_ages, type = "prob", closure_age = 130, start_fit_age = 75, smoothing = FALSE, years = NULL) {


  if (is.null(years)) {
    col_names <- colnames(rates)
  }

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

  if (is.vector(qxy_arr) | is.matrix(qxy_arr)) {
    completed_qxy_arr <- DG_mat(qxy_arr)
  } else {
    completed_qxy_arr <- arr_apply(qxy_arr, DG_mat)
  }

  rownames(completed_qxy_arr) <- as.character(c(kept_ages, old_ages))
  colnames(completed_qxy_arr) <- if (is.null(years)) col_names else as.character(years)

  completed_qxy_arr

}

#' Kannisto Method of Age Completion
#'
#' Implements the Kannisto method of age completion for old ages.
#'
#' @param rates
#' matrix or vector of mortality rates with age (on the rows) and calendar year
#' (on the columns). Vector is equivalent to a matrix with a single column
#' @param ages
#' vector of ages for `rates`
#' @param old_ages
#' vector of old ages for which `rates` is to be completed for
#' @param fitted_ages
#' vector of ages for which model is fitted on
#' @param type
#' character string representing the type of mortality rate supplied. Takes the
#' following values: "central" for central death rates, "prob" for 1-year death
#' probabilities, "force" for force of mortality
#' @param closure_age
#' maximum life span
#' @param years
#' optional vector of years for `rates`. If not supplied, then the column names
#' of `rates` will be preserved
#'
#' @return
#' matrix of force of mortality for all ages and calendar years
#'
#' @export
#'
#' @examples
#'
kannisto <- function(rates, ages, old_ages, fitted_ages, type = "force", closure_age = 130, years = NULL) {

  if (is.null(years)) {
    col_names <- colnames(rates)
  }

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
      completed_muxy[closure_age - ages[1] + 1, ] <- 20
    }

    completed_muxy

  }

  if (is.vector(muxy_arr) | is.matrix(muxy_arr)) {
    completed_muxy_arr <- kannisto_mat(muxy_arr)
  } else {
    completed_muxy_arr <- arr_apply(muxy_arr, kannisto_mat)
  }

  rownames(completed_muxy_arr) <- as.character(c(kept_ages, old_ages))
  colnames(completed_muxy_arr) <- if (is.null(years)) col_names else as.character(years)

  completed_muxy_arr



}
