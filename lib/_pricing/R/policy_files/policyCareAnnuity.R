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
cf_care_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    increase <- policy$increase
    benefit <- policy$benefit
    minimum <- policy$min

    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1) {     # while PH is not dead

        # Get base level benefit for being alive
        cf[i] <- benefit[1]

        # Add additional benefits from LTC: state[i] = 1 (M), 2 (D), 3(MD)
        if (state[i] > 0) {
            cf[i] <- cf[i] + benefit[state[i] + 1]
        }

        # For flat-rate increases of benefits
        benefit <- benefit * (1 + increase)

        i <- i + 1
    }

    # Account for any cashflows associated with minimum guarantees
    while (i <= max(minimum)) {

        # Create mask for all policies with min guarantees for current period
        mask <- ifelse((minimum - i + 1) > 0, as.logical(minimum), 0)
        cf[i] <- sum(mask * benefit)

        # For flat-rate increases of benefits
        benefit <- benefit * (1 + increase)

        i <- i + 1
    }

    return(cf)
}

# For indexed benefits (e.g. inflation)
# benefit <- benefit * (1 + index[i])
