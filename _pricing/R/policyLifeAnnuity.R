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
cf_life_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    increase <- policy$increase
    benefit <- policy$benefit
    d <- policy$defer

    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1) {     # while PH is not dead

        # Get benefit if alive after deferment period
        cf[i] <- ifelse (i <= d, 0, benefit)

        # For flat-rate increase
        benefit <- benefit * (1 + increase)

        i <- i + 1
    }

    return(cf)
}

# For indexed benefits (e.g. inflation)
# benefit <- benefit * (1 + index[i])

