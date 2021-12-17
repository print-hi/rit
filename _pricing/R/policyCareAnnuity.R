cf_care_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    benefit <- policy$benefit
    defermt <- policy$defer

    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1) {     # while PH is not dead

        if (i == 1) {
            cf[i] <- calculate_price_la()
        }

        i <- i + 1
    }

    return(cf)
}
