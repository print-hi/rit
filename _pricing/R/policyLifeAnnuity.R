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
    benefit <- policy$benefit
    defermt <- policy$defer

    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1) {     # while PH is not dead

        if (i == 1) {
            cf[i] <- -1 * calculate_price_LA()
        } else if (i <= d) {
            cf[i] <- 0
        } else {
            cf[i] <- policy$benefit
        }

        i <- i + 1
    }

    return(cf)
}


# Should this be calculated in the main wrapper? and passed in via policy
calculate_price_LA <- function() {

}
