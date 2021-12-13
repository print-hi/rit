# Constant Force of Mortality ---------------------------------------------
mu2q <- function(rates) return(1 - exp(-rates))
q2mu <- function(rates) return(-log(1 - rates))

m2q <- function(rates) return(1 - exp(-rates))
q2m <- function(rates) return(-log(1 - rates))


#' Convert mortality rates
#'
#' Converts between central death rates, 1-year death probabilities and force of
#' mortality.
#'
#' Implementation assumes a constant force of mortality for fractional ages,
#' resulting in the following conversions:
#' \deqn{q_{xy} = 1 - e^{-\mu_{xy}}}
#' \deqn{\mu_{xy} = m_{xy}}
#' \deqn{m_{xy}  = -log(1 - q_{xy})}
#'
#' @param rates
#' matrix containing the rates
#' @param from
#' character string representing the type of mortality rate to be converted
#' from. Takes the following values: "central" for central death rates, "prob"
#' for 1-year death probabilities, "force" for force of mortality.
#' @param to
#' character string representing the type of mortality rate to be converted to.
#' Takes the same values as the parameter `from`
#'
#' @return
#' matrix of the same size as `rates` containing the converted rates
#'
#' @export
#'
#' @examples
#' A <- matrix(c(0.02, 0.04, 0.03, 0.05), nrow = 2, ncol = 2, byrow = TRUE)
#' rate2rate(A, from = "prob", to = "force")
rate2rate <- function(rates, from, to) {
  if (from == to) return(rates)
  else if (from == "central" & to == "force") return(rates)
  else if (from == "central" & to == "prob") return(m2q(rates))
  else if (from == "force" & to == "central") return(rates)
  else if (from == "force" & to == "prob") return(mu2q(rates))
  else if (from == "prob" & to == "central") return(q2m(rates))
  else if (from == "prob" & to == "force") return(q2mu(rates))
  else stop("invalid arguments for rate conversion")
}
