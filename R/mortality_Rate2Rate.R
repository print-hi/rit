#' Convert mortality rates
#'
#' Converts between central death rates, 1-year death probabilities and force of
#' mortality.
#'
#' Implementation assumes a constant force of mortality for fractional ages,
#' resulting in the following conversions:
#' \deqn{q_{x, y} = 1 - e^{-\mu_{x, y}}}
#' \deqn{\mu_{x, y} = m_{x, y}}
#' \deqn{m_{x, y}  = -log(1 - q_{x, y})}
#'
#' @param rates
#' vector, matrix or 3D array of mortality rates
#' @param from
#' character string representing the type of mortality rate to be converted
#' from. Takes the following values: "central" for central death rates, "prob"
#' for 1-year death probabilities, "force" for force of mortality.
#' @param to
#' character string representing the type of mortality rate to be converted to.
#' Takes the same values as the parameter `from`
#'
#' @return
#' vector, matrix or 3D array of converted mortality rates
#'
#' @export
#'
rate2rate <- function(rates, from, to) {

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

  mu2q <- function(rates) return(1 - exp(-rates))
  q2mu <- function(rates) return(-log(1 - rates))
  m2q <- function(rates) return(1 - exp(-rates))
  q2m <- function(rates) return(-log(1 - rates))

  type <- c("central", "prob", "force")

  if (!is.element(from, type) | !is.element(to, type)) stop("invalid input for conversion")
  else if (from == to) return(rates)
  else if (from == "central" & to == "force") return(rates)
  else if (from == "central" & to == "prob") return(m2q(rates))
  else if (from == "force" & to == "central") return(rates)
  else if (from == "force" & to == "prob") return(mu2q(rates))
  else if (from == "prob" & to == "central") return(q2m(rates))
  else if (from == "prob" & to == "force") return(q2mu(rates))
  else stop("invalid input for conversion")
}
