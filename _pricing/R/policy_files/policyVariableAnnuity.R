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
cf_variable_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    value <- policy$value
    g_fee <- policy$g_fee
    contract_length <- policy$length
    withdraw_prop <- policy$withdraw_prop

    #s_fee <- policy$s_fee # not used in static model

    # Initialize output vector
    cf <- rep(0, times = length(state))

    # Tracks Total amount that can be withdrawn + Value of portfolio account
    total_remaining <- value
    max_withdraw <- total_remaining * withdraw_prop

    # Start account value at time 1, as no time 0 cashflow
    account_value <- value * data$stock[1]

    i <- 2
    while (state[i] != -1) {     # while PH is not dead

        # Compound account value - expenses for withdrawl guarantee
        account_value <- account_value * data$stock[i] * exp(-g_fee)

        # Calculate withdraw limit for current period
        withdraw_limit <- min(max_withdraw, total_remaining)

        if (t >= contract_length) {
            cf[i] = account_value
            break
        } else {
            cf[i] = withdraw_limit
            account_value <- max(account_value - withdraw_limit, 0)
            total_remaining <- max(total_remaining - withdraw_limit, 0)
        }

        i <- i + 1
    }

    return(cf)
}

# NOTE: not used as static method withdraws every period -> no step-ups can
# be applied
#
# withdrawl = FALSE
#
# Calculate max withdrawl for each time period
# if (is.element((i - 1), step_time)) {
#     # Step up max withdrawl for future time periods by relevant factor
#     total_remaining <- total_remaining * step_size[match((i - 1), x)]
# }

# # Update max yearly withdrawl
# max_withdraw <- total_remaining * withdraw_prop
