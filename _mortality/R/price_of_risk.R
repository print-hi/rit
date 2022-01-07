
#' Convert death probabilities to survival rates
#'
#' Converts 1-year death probabilities to the associated survival function.
#' @param qx
#' vector, matrix or 3D array of 1-year death probabilities with age
#' (on the rows) and calendar year (on the columns)
#'
#' @return
#' survival rates in the same format as `qx`
#'
#' @export
#'
#' @examples
#'
q2survival <- function(qx, ages, target_age = NULL, years = NULL) {


  if (is.null(years)) {
    col_names <- colnames(qx)
  }

  if(is.null(target_age)) {
    target_age <- ages[1]
  }

  if(target_age == ages[1]) {
    px <- 1 - qx
  } else {
    px <- 1 - tail(qx, ages[1] - target_age)
  }

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
#' Q-measure according to the specified distortion risk measure
#'
#' The distortion risk measures and their strings are as follows
#' * "wang": Wang Transform
#' * "ph": Proportional Hazard Transform
#' * "dp": Dual-power Transform
#' * "dadp": Denneberg's Absolute Deviation Principle
#' * "exp": Exponential Transform
#' * "log": Logarithmic Transform
#'
#' @param StP
#' vector, matrix or 3D array of the survival function under the P-measure with
#' survival time (on the rows) and calendar year (on the columns)
#' @param method
#' character string representing the distortion risk measure to be used. See "Details".
#' @param lambda
#' parameter associated with the distortion risk measure
#' @param t
#' optional vector of survival time
#' @param ages
#' optional vector of ages
#' @param years
#' optional vector of years
#'
#' @return
#' the transformed survival function under the Q-measure
#' @export
#'
#' @examples
#'
survivalP2Q <- function(StP, method, lambda, t = NULL, ages = NULL, years = NULL) {

  # Need to make sure values only between 0 and 1

  # Defining distortion functions
  wang <- function(x, lam) {
    if (lam < 0) stop("invalid lambda value")
    return(1 - pnorm(qnorm(1 - x) - lam))
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

  type <- c("wang", "ph", "dp", "gp", "dadp", "exp", "log")

  if (!is.element(method, type)) stop("invalid method type")
  else if (method == "wang") distort <- wang
  else if (method == "ph") distort <- ph
  else if (method == "dp") distort <- dp
  else if (method == "gp") distort <- gp
  else if (method == "dadp") distort <- dadp
  else if (method == "exp") distort <- exp_tfm
  else if (method == "log") distort <- log_tfm

  distort(StP, lambda)

}

pdfP2Q <- function(StP, method, lambda, t = NULL, ages = NULL, years = NULL) {

  type <- c("canon", "esscher")

  if (!is.element(method, type)) stop("invalid input for conversion")

}
