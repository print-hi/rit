#' Convert death probabilities to survival function
#'
#' Converts mortality rates to the associated survival function.
#'
#' The survival function has survival time (starting from 0) on the rows.
#'
#' @param rates
#' vector, matrix or 3D array of mortality rates with age
#' (on the rows) and calendar year or cohort (on the columns) and simulation
#' number (3rd dimension)
#' @param ages
#' vector of ages for `rates`
#' @param from
#' character string representing the type of mortality rate to be converted
#' from. Takes the following values: "central" for central death rates, "prob"
#' for 1-year death probabilities, "force" for force of mortality.
#' @param init_age
#' initial age for which the survival function is to be calculated at. If not provided,
#' the survival function will be calculated for the smallest age supplied in `ages`
#' @param years
#' optional vector of years for `rates`. If not supplied, then the column names
#' of `rates` will be preserved
#'
#' @return
#' associated survival function as a 3D array if `rates` is an array, or as a matrix otherwise
#'
#' @export
#'
rate2survival <- function(rates, ages, from = "prob", init_age = NULL, years = NULL) {

# Flagging errors ---------------------------------------------------------

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

  if (from == "prob" & any(rates > 1, na.rm = T)) {
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

  # from
  if (!is.element(from, c("central", "prob", "force"))) {
    stop("from must be 'central', 'prob' or 'force'")
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

  # Converting to 1-year death probabilities
  qx <- rate2rate(rates, from, "prob")

  # Converting to 1-year survival probabilities

  if(is.null(init_age)) {
    init_age <- ages[1]
  } else if (!is.element(init_age, ages)) {
    stop("initial age must be in ages")
  }

  if(init_age == ages[1]) {
    px <- 1 - qx
  } else {
    px <- 1 - utils::tail(qx, ages[1] - init_age)
  }

  # Deal with R numerical error
  px[dplyr::near(px, 0)] <- 0

  # Calculating survival function
  if (is.vector(px)) {
    St <- rbind(1, matrix(cumprod(px)))
  } else if (is.matrix(px)) {
    St <- rbind(1, apply(px, 2, cumprod))
  } else if (is.array(px)) {
    St <- arr_apply(px, function(x) rbind(1, apply(x, 2, cumprod)))
  }

  rownames(St) <- as.character(0:(ages[length(ages)] - init_age + 1))
  colnames(St) <- if (is.null(years)) colnames(qx) else as.character(years)

  return(St)

}

#' Convert survival function to death probabilities
#'
#' Converts the survival function to the associated mortality rates.
#'
#' @param surv
#' vector, matrix or 3D array of the survival function with survival time starting from 0
#' (on the rows) and calendar year or cohort (on the columns) and simulation number (3rd dimension)
#' @param ages
#' vector of desired ages for the resulting 1-year death probabilities
#' @param to
#' character string representing the type of mortality rate to be converted
#' to. Takes the following values: "central" for central death rates, "prob"
#' for 1-year death probabilities, "force" for force of mortality.
#' @param years
#' optional vector of years for `surv`. If not supplied, then the column names
#' of `surv` will be preserved
#'
#' @return
#' associated mortality rates in the same format as `surv`
#' @export
#'
survival2rate <- function(surv, ages, to = "prob", years = NULL) {

# Flagging Errors ---------------------------------------------------------
  # surv
  if (!is.vector(surv) & !is.matrix(surv) & !(is.array(surv) & length(dim(surv)) == 3)) {
    stop("survival function must be a vector, 2D matrix or a 3D array")
  }

  if (!is.numeric(surv)) {
    stop("survival function must be numeric")
  }

  if (any(surv < 0, na.rm = T)) {
    stop("survival function must be non-negative")
  }

  if (any(surv > 1, na.rm = T)) {
    stop("survival function must be less than or equal to 1")
  }

  # ages
  if (length(ages) != NROW(surv) - 1) {
    stop("length of ages and survival times do not match")
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

  # to
  if (!is.element(to, c("central", "prob", "force"))) {
    stop("to must be 'central', 'prob' or 'force'")
  }

  # years
  if (!is.null(years)) {
    if (length(years) != NCOL(surv)) {
      stop("length of years must be equal to number of columns of the survival function")
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

  px <- ifelse(utils::head(surv, -1) == 0, 0, utils::tail(surv, -1) / utils::head(surv, -1))
  qx <- 1 - px

  rates <- rate2rate(qx, from = "prob", to = to)


  if (!is.vector(rates)) {
    rownames(rates) <- as.character(ages)
    colnames(rates) <- if (is.null(years)) colnames(surv) else as.character(years)
  }

  return(rates)

}


#' Survival Function Transformation
#'
#' Transforms the survival function from the real world P-measure to the risk-neutral
#' Q-measure according to the specified risk-neutral principles.
#'
#' The risk-neutral principles and their corresponding strings and valid lambda range
#' are as follows:
#' * "wang", \eqn{\lambda \ge 0}: Wang Transform,
#' * "ph", \eqn{\lambda \ge 1}: Proportional Hazard Transform
#' * "dp", \eqn{\lambda \ge 1}: Dual-power Transform
#' * "gp", \eqn{0 \le \lambda \le 1}: Gini Principle
#' * "dadp", \eqn{0 \le \lambda \le 1}: Denneberg's Absolute Deviation Principle
#' * "exp", \eqn{\lambda > 0}: Exponential Transform
#' * "log", \eqn{\lambda > 0}: Logarithmic Transform
#' * "canon", \eqn{\lambda > 0}: Univariate Canonical Valuation
#' * "esscher", \eqn{\lambda > 0}: Esscher Transform
#'
#' The first seven principles are distortion risk measures and act on the survival function.
#' The last two principles act on the probability density function.
#'
#' @param StP
#' vector, matrix or 3D array of the survival function under the P-measure with
#' survival time (on the rows) and calendar year or cohort (on the columns) and simulation
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
survivalP2Q <- function(StP, method, lambda) {

# Flagging Errors ---------------------------------------------------------

  # StP
  if (!is.vector(StP) & !is.matrix(StP) & !(is.array(StP) & length(dim(StP)) == 3)) {
    stop("survival function must be a vector, 2D matrix or a 3D array")
  }

  if (!is.numeric(StP)) {
    stop("survival function must be numeric")
  }

  if (any(StP < 0, na.rm = T)) {
    stop("survival function must be non-negative")
  }

  if (any(StP > 1, na.rm = T)) {
    stop("survival function must be less than or equal to 1")
  }

  # method
  valid_methods <- c("wang", "ph", "dp", "gp", "dadp", "exp", "log", "canon", "esscher")
  if (!is.element(method, valid_methods)) {
    stop("invalid risk distortion method")
  }

  if (!is.numeric(lambda)) {
    stop("lambda must be numeric")
  }

# Implementation -----------------------------------------------------------

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
  pdf_type <- c("canon", "esscher")

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

    if (lambda <= 0) stop("invalid lambda value")

    pdfP2Q <- function(StP_mat) {

      time <- 0:(nrow(StP_mat) - 1)
      # Calculating pdf
      ftP <- rbind(0, diff(1 - StP_mat))
      rownames(ftP) <- as.character(time)
      # ftP will sum up to 1 if survival function starts from 1 and ends at 0
      stopifnot(nrow(StP_mat) == nrow(ftP))

      # Canonical valuation

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
