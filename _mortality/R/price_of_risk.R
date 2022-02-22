
#' Convert death probabilities to survival rates
#'
#' Converts 1-year death probabilities to the associated survival function.
#' @param qx
#' vector, matrix or 3D array of 1-year death probabilities with age
#' (on the rows) and calendar year (on the columns)
#'
#' @param ages
#' vector of ages for `qx`
#'
#' @param target_age
#' age for which the survival function is to be calculated at. If not provided,
#' the survival function will be calculated for the smallest age supplied in `ages`
#'
#' @param years
#' optional vector of years for `qx`. If not supplied, then the column names
#' of `qx` will be preserved
#'
#' @return
#' survival rates in the same format as `qx`
#'
#' @export
#'
#' @examples
#'
q2survival <- function(qx, ages, target_age = NULL, years = NULL) {
  # TODO: Check target_age is in ages

  if (is.null(years)) {
    col_names <- colnames(qx)
  }

  if(is.null(target_age)) {
    target_age <- ages[1]
  }

  # Converting to 1-year survival probabilities
  if(target_age == ages[1]) {
    px <- 1 - qx
  } else {
    px <- 1 - utils::tail(qx, ages[1] - target_age)
  }

  # Calculating survival function
  if (is.vector(px)) {
    St <- rbind(1, matrix(cumprod(px)))
  } else if (is.matrix(px)) {
    St <- rbind(1, apply(px, 2, cumprod))
  } else if (is.array(px)) {
    St <- arr_apply(px, function(x) rbind(1, apply(x, 2, cumprod)))
  }

  rownames(St) <- as.character(0:(ages[length(ages)] - target_age + 1))
  colnames(St) <- if (is.null(years)) col_names else as.character(years)

  return(list(surv = St, age = target_age))

}


#' Survival Function Transformation
#'
#' Transforms the survival function from the real world P-measure to the risk-neutral
#' Q-measure according to the specified risk-neutral principles.
#'
#' The risk-neutral principles and their corresponding strings are as follows:
#' * "wang": Wang Transform
#' * "ph": Proportional Hazard Transform
#' * "dp": Dual-power Transform
#' * "gp": Gini Principle
#' * "dadp": Denneberg's Absolute Deviation Principle
#' * "exp": Exponential Transform
#' * "log": Logarithmic Transform
#' * "canon": Univariate Canonical Valuation
#' * "esscher": Esscher Transform
#'
#' The first seven principles are distortion risk measures and act on the survival function.
#' The last two principles act on the probability density function.
#'
#' @param StP
#' vector, matrix or 3D array of the survival function under the P-measure with
#' survival time (on the rows) and calendar year (on the columns) and simulation
#' number (3rd dimension)
#' @param method
#' character string representing the distortion risk measure to be used. See "Details".
#' @param lambda
#' parameter associated with the distortion risk measure
#'
#' @return
#' the transformed survival function under the specified Q-measure
#' @export
#'
#' @examples
#'
survivalP2Q <- function(StP, method, lambda) {

  # TODO: make sure values only between 0 and 1

  # Defining survival function distortion functions
  wang <- function(x, lam) {
    if (lam < 0) stop("invalid lambda value")
    return(1 - stats::pnorm(stats::qnorm(1 - x) - lam))
  }

  ph <- function(x, lam) {
    if (lam < 1) stop("invalid lambda value")
    return(x^(1 / lam))
  }

  dp <- function(x, lam) {
    if (lam < 1) stop("invalid lambda value")
    return(1 - (1 - x)^lam)
  }

  gp <- function(x, lam) {
    if (lam < 0 | lam > 1) stop("invalid lambda value")
    return((1 + lam) * x - lam * x^2)
  }

  dadp <- function(x, lam) {
    if (lam < 0 | lam > 1) stop("invalid lambda value")
    return(ifelse(x < 0.5, (1 + lam) * x, lam + (1 - lam) * x))
  }

  exp_tfm <- function(x, lam) {
    if (lam <= 0) stop("invalid lambda value")
    return((1 - exp(-lam * x)) / (1 - exp(-lam)))
  }

  log_tfm <- function(x, lam) {
    if (lam <= 0) stop("invalid lambda value")
    return(log(1 + lam * x) / log(1 + lam))
  }

  survival_type <- c("wang", "ph", "dp", "gp", "dadp", "exp", "log")
  pdf_type <- type <- c("canon", "esscher")

  if (is.element(method, survival_type)) {
    if (method == "wang") distort <- wang
    else if (method == "ph") distort <- ph
    else if (method == "dp") distort <- dp
    else if (method == "gp") distort <- gp
    else if (method == "dadp") distort <- dadp
    else if (method == "exp") distort <- exp_tfm
    else if (method == "log") distort <- log_tfm

    return(distort(StP, lambda))
  } else if (is.element(method, pdf_type)) {
    # Risk-adjusted pdf is identical for univariate canonical valuation and
    # esscher transform


    pdfP2Q <- function(StP_mat) {
      # Calculating pdf
      ftP <- rbind(0, diff(1 - StP_mat))
      # TODO: Check if ftP need to sum up to 1?
      stopifnot(nrow(StP_mat) == nrow(ftP))

      # Canonical valuation
      time <- 0:(nrow(StP_mat) - 1)
      num <- exp(lambda * time) * ftP
      denom <- apply(num, 2, sum)
      ftQ <- num / outer(rep(1, nrow(StP_mat)), denom)
      StQ_mat <- 1 - apply(ftQ, 2, cumsum)

      return(StQ_mat)
    }

    # Treating vectors, matrices and arrays differently
    if (is.vector(StP)) {
      StQ <- pdfP2Q(as.matrix(StP))
    } else if (is.matrix(StP)) {
      StQ <- pdfP2Q(StP)
    } else if (is.array(StP)) {
      StQ <- arr_apply(StP, pdfP2Q)
    }

    stopifnot(dim(StP) == dim(StQ))
    dimnames(StQ) <- dimnames(StP)

    return(StQ)

  } else {
    stop("invalid method type")
  }

}

