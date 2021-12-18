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

    prem <- calculate_fair_premium_RM()

    value <- policy$value

    # Accrue interest of loan and house value
    i <- 1
    while (state[i] == 0) {     # while PH is healthy (i.e. not dead or sick)
        loan <- loan * exp(data$intrs[i] + policy$margin + prem)
        value <- value * data$house[i]
        i <- i + 1
    }

    # Calculate any profits leftover from sale
    excess <- (1 - policy$cost) * value - loan
    # cf[i] <- max(excess, 0)

    # For debugging
    cf[i] <- excess

    return(cf)
}
