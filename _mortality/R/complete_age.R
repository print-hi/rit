#' Coale and Kisker Method of Age Completion
#'
#' Implements the Coale and Kisker method of age completion.
#'
#'
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
  # write tests to ensure inputs are in correct format
  # e.g. ages and years must be continuous vector,
  # dimensions of rates needs to match those of ages and years

  # need to import rate2rate function?
  if (type != "central") {
    mxy <- rate2rate(rates, from = type, to = "central")
  } else {
    mxy <- rates
  }

  mxy <- as.matrix(mxy)
  # convert vector into matrix if needed

  boundary_age <- old_ages[1] - 1
  m_boundary <- mxy[boundary_age - ages[1] + 1, ]
  k_boundary <- log(m_boundary/mxy[boundary_age - ages[1], ])
  n <- length(old_ages)
  s <- 2 / (n * (n+1)) * (n * k_boundary - log(m_end/m_boundary))


  old_kxy <- outer(rep(1, length(old_ages)), k_boundary) - outer(old_ages - boundary_age, s)
  # outer is used to create matrix of appropriate dimensions for element-wise
  # matrix multiplication
  rownames(old_kxy) <- as.character(old_ages)


  old_kxy_cumsum <- apply(old_kxy, 2, cumsum)

  old_mxy <- outer(rep(1, length(old_ages)), m_boundary) * exp(old_kxy_cumsum)

  rownames(old_mxy) <- as.character(old_ages)


  kept_mxy <- as.matrix(mxy[-(old_ages - ages[1] + 1), ])
  # Ensure matrix with 1 column is still of type matrix after subsetting
  completed_mxy <- rbind(kept_mxy, old_mxy)
  rownames(completed_mxy) <- c(rownames(kept_mxy), rownames(old_mxy))
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
  # add method later
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
