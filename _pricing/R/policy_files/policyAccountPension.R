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
cf_account_based_pension <- function(policy, state, data) {

    # Extract relevant policy variables
    balance <- policy$bal
    expense <- policy$exp

    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1) {    # while PH is not dead

        # Withdraw yearly expense from account
        balance <- balance - expense

        # Reduce withdrawl amount + exit loop if fund value is negative
        if (balance < 0) {
            cf[i] <- expense + balance
            break
        }

        # Record cashflow to output vector
        cf[i] <- expense

        # Update balance and yearly expense
        balance <- balance * (1 + data$stock[i])
        expense <- expense * (1 + data$infla[i])

        i <- i + 1
    }

    return(cf)
}


# Accounting for any remaining funds in account ??
# if (i > length(state)) {
#     cf[length(state)] <- cf[length(state)] + balance
# }
# else {
#     cf[i] <- balance
# }


# Assumptions made: cash flows end when account balance is negative
# ? withdrawl all for max age (e.g. goes to family)
