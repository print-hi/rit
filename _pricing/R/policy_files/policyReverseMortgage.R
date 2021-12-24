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

    # Extract relevant policy variables
    LVR <- policy$LVR
    cost <- policy$cost
    value <- policy$value
    margin <- policy$margin

    # Initialize output vector
    cf <- rep(0, times = length(state))

    # Get loan amount for policyholder
    loan <- LVR * value
    cf[1] <- loan

    # Accrue interest of loan + Appreciate house value
    i <- 1
    while (state[i] == 0) {     # while PH is healthy (i.e. not dead or sick)

        # Compound loan value over 1 year period
        loan <- loan * exp(data$rfree[i] + margin)

        # Update house value after 1 year period
        value <- value * data$house[i]

        i <- i + 1
    }

    # Calculate cashflow from sale (includes negative value)
    cf[i] <- (1 - cost) * value - loan

    return(cf)
}

# consider using { state[i] > -1 & state[i] < 3 } for 5 state model
