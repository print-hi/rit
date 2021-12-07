#' Coale and Kisker Method of Age Completion
#'
#' Implements the Coale and Kisker method of age completion.
#'
#' @param rates mortality rates in a rectangular array with ages (on the rows)
#'   and calendar year (on the columns). Can be a matrix or a vector
#' @param type specifies the type of rates supplied. Takes the following values:
#'   "central" for central death rates, "prob" for 1-year death probabilities,
#'   "force" for force of mortality
#' @param ages age vector for \code{rates}
#' @param years year vector for \code{rates}
#' @param old_ages old ages to be completed for
#' @param m_end a constant or vector specifying the central death rates at the final age
#'
#' @return Central death rates in a rectangular array for all ages and calendar years
#' @export
#'
#' @examples
#'
CK <- function(rates, type, ages, years, old_ages, m_end) {
  # TODO: Write tests for inputs, e.g.
    # ages and years must be continuous vector
    # dimensions of rates needs to match those of ages and years

  # Convert to central death rates
  if (type != "central") {
    mxy <- rate2rate(rates, from = type, to = "central")
  } else {
    mxy <- rates
  }

  # Convert vector to matrix if necessary
  mxy <- as.matrix(mxy)

  # Obtaining relevant ages
  boundary_age <- old_ages[1] - 1
  kept_ages <- ages[1]:boundary_age

  # Calculating parameter sy
  m_boundary <- mxy[boundary_age - ages[1] + 1, ]
  k_boundary <- log(m_boundary/mxy[boundary_age - ages[1], ])
  n <- length(old_ages)
  sy <- 2 / (n * (n+1)) * (n * k_boundary - log(m_end/m_boundary))


  old_kxy <- outer(rep(1, length(old_ages)), k_boundary) - outer(old_ages - boundary_age, sy)
  # Outer creates matrix of appropriate dimensions for element-wise matrix multiplication

  # Completion of old ages
  old_kxy_cumsum <- apply(old_kxy, 2, cumsum)
  old_mxy <- outer(rep(1, length(old_ages)), m_boundary) * exp(old_kxy_cumsum)

  # Preparing final output
  kept_mxy <- as.matrix(mxy[1:(boundary_age - ages[1] + 1), ]) # as.matrix converts
  # vector to matrix if necessary
  completed_mxy <- rbind(kept_mxy, old_mxy)
  rownames(completed_mxy) <- as.character(c(kept_ages, old_ages))
  colnames(completed_mxy) <- as.character(years)

  completed_mxy
}

#' Denuit and Goderniaux Method of Age Completion
#'
#' Implements the Denuit and Goderniaux method of age completion.
#'
#' @param rates mortality rates in a rectangular array with ages (on the rows)
#'   and calendar year (on the columns)
#' @param type specifies the type of rates supplied. Takes the following values:
#'   "central" for central death rates, "prob" for 1-year death probabilities,
#'   "force" for force of mortality
#' @param ages age vector for \code{rates}
#' @param years year vector for \code{rates}
#' @param old_ages old ages to be completed for
#' @param closure_age age for which closure constraint is applied
#' @param min_fit_age model is fitted starting from this age
#' @param smoothing logical value indicating if smoothing is to be applied
#'
#' @return 1-year death probabilities in a rectangular array for all ages and calendar years
#' @export
#'
#' @examples
#'
DG <- function(rates, type, ages, years, old_ages, closure_age = 130, min_fit_age = 75, smoothing = FALSE) {
  # TODO: Implement smoothing
  # TODO: Write tests for inputs
  # Rates data needs to be cleaned prior to input i.e. probabilities > 0 so log is defined

  # Convert to death probabilities
  if (type != "prob") {
    qxy <- rate2rate(rates, from = type, to = "prob")
  } else {
    qxy <- rates
  }

  # Convert vector to matrix if necessary
  qxy <- as.matrix(qxy)

  # Creating data frame to fit log-quadratic model
  input_df <- as.data.frame(qxy)
  df_fit <- as.data.frame(input_df[ages >= min_fit_age,])

  # Obtaining relevant ages
  boundary_age <- old_ages[1] - 1
  kept_ages <- ages[1]:boundary_age
  fitted_ages <- min_fit_age:old_ages[length(old_ages)]

  # Helper function to fit log-quadratic model on a vector
  DG_fit <- function(qx) {

    log_quad_fit <- lm(log(qx) ~ I((closure_age - fitted_ages)^2) - 1)
    coefficients(log_quad_fit)
  }

  # Calculating regression coefficient c
  cy <- as.numeric(lapply(df_fit, DG_fit))
  cxy <- outer(rep(1, length(old_ages)), cy) # Outer creates matrix of
  # appropriate dimensions for element-wise matrix multiplication

  # Completion for old ages
  old_qxy <- as.matrix(exp(cxy * (closure_age - old_ages)^2)) # Convert vector
  # to matrix if necessary

  # Preparing final output
  kept_qxy <- as.matrix(qxy[1:(boundary_age - ages[1] + 1), ])
  completed_qxy <- rbind(kept_qxy, old_qxy)
  rownames(completed_qxy) <- as.character(c(kept_ages, old_ages))
  colnames(completed_qxy) <- as.character(years)

  completed_qxy
}

#' Kannisto Method of Age Completion
#'
#' Implements the Kannisto method of age completion
#'
#' @param rates mortality rates in a rectangular array with ages (on the rows)
#'   and calendar year (on the columns)
#' @param type specifies the type of rates supplied. Takes the following values:
#'   "central" for central death rates, "prob" for 1-year death probabilities,
#'   "force" for force of mortality
#' @param ages age vector for \code{rates}
#' @param years year vector for \code{rates}
#' @param old_ages old ages to be completed for
#' @param fitted_ages ages for which parameters are to be estimated from
#'
#' @return Force of mortality in a rectangular array for all ages and calendar years
#' @export
#'
#' @examples
#'
kannisto <- function(rates, type, ages, years, old_ages, fitted_ages) {
  # add method later
}
