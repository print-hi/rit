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
cf_pooled_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    benefit <- policy$benefit
    size <- policy$size
    interest <- policy$interest

    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1) {     # while PH is not dead

        # Get benefit if alive
        cf[i] <- benefit

        # Skip rate calculations for final year
        if (i + 1 > length(state)) {
            break
        }

        # Calculate expected and realized survivorship rates
        suv_e <- data$pool_e[i + 1] / data$pool_e[i]
        suv_r <- data$pool_r[i + 1] / data$pool_r[i]

        # Increase benefit by ratio of rates
        benefit <- benefit * suv_e / suv_r

        # Compound realized + discount expected rates
        benefit <- benefit * (1 + data$stock) / (1 + interest)

        i <- i + 1
    }

    return(cf)
}

# For indexed benefits (e.g. inflation)
# benefit <- benefit * (1 + index[i])

calculate_unit_annuity_due <- function(time, interest, mortality_expected) {

    value <- 0
    discount <- 1
    for (i in seq(time, length(mortality_expected))) {
        value <- value + (discount * mortality_expected[i])
        discount <- discount / (1 + interest)
    }

    value <- value/mortality_expected[1]

    return(value)
}
