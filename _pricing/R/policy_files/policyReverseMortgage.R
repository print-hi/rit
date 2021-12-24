#' Title
#'
#' @param policy
#' @param state
#' @param data
#'
#' @return
#' @export
#'
#' @examples
cf_reverse_mortgage <- function(policy, state, data) {

    # Initialize output vector
    cf <- rep(0, times = length(state))

    # Get loan amount for policyholder
    loan <- policy$LVR * policy$value
    cf[1] <- loan

    # Track house value over time
    value <- policy$value

    # Accrue interest of loan and Appreciate house value
    i <- 1
    while (state[i] == 0) {     # while PH is healthy (i.e. not dead or sick)
        loan <- loan * exp(data$intrs[i] + policy$margin + prem)
        value <- value * data$house[i]
        i <- i + 1
    }

    # Calculate cashflow from sale (includes negative value)
    cf[i] <- (1 - policy$cost) * value - loan

    return(cf)
}

# consider using { state[i] > -1 & state[i] < 3 } for 5 state model
