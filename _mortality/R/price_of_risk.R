#' Convert death probabilities to survival function
#'
#' Converts 1-year death probabilities to the associated survival function.
#'
#' The survival function has survival time (starting from 0) on the rows.
#'
#' @param qx
#' vector, matrix or 3D array of 1-year death probabilities with age
#' (on the rows) and calendar year (on the columns) and simulation
#' number (3rd dimension)
#' @param ages
#' vector of ages for `qx`
#' @param init_age
#' initial age for which the survival function is to be calculated at. If not provided,
#' the survival function will be calculated for the smallest age supplied in `ages`
#' @param years
#' optional vector of years for `qx`. If not supplied, then the column names
#' of `qx` will be preserved
#'
#' @return
#' associated survival function as a 3D array if `qx` is an array, or as a matrix otherwise
#'
#' @export
#'
#' @examples
#'
q2survival <- function(qx, ages, init_age = NULL, years = NULL) {
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

  return(list(surv = St, age = init_age))

}

#' Convert survival function to death probabilities
#'
#' Converts the survival function to the associated 1-year death probabilities.
#'
#' @param surv
#' vector, matrix or 3D array of the survival function with survival time starting from 0
#' (on the rows) and calendar year (on the columns) and simulation number (3rd dimension)
#' @param ages
#' vector of desired ages for the resulting 1-year death probabilities
#' @param years
#' optional vector of years for `surv`. If not supplied, then the column names
#' of `surv` will be preserved
#'
#' @return
#' associated 1-year death probabilities in the same format as `surv`
#' @export
#'
#' @examples
#'
survival2q <- function(surv, ages, years = NULL) {

  if(!is.null(ages)) {
    stopifnot(length(ages) == NROW(surv) - 1)
  }

  if(!is.null(years)) {
    stopifnot(length(years) == NCOL(surv))
  }

  px <- ifelse(utils::head(surv, -1) == 0, 0, utils::tail(surv, -1) / utils::head(surv, -1))
  qx <- 1 - px


  if (!is.vector(qx)) {
    rownames(qx) <- as.character(ages)
    colnames(qx) <- if (is.null(years)) colnames(surv) else as.character(years)
  }


  return(qx)

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

