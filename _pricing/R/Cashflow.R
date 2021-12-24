# ---------------------------------------------------------------------------- #
# --------------------------- Account Based Pension -------------------------- #
# ---------------------------------------------------------------------------- #

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

    # Initialize output vector
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



# ---------------------------------------------------------------------------- #
# ------------------------------- Care Annuity ------------------------------- #
# ---------------------------------------------------------------------------- #

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

    # Initialize output vector
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


# ---------------------------------------------------------------------------- #
# ------------------------------- Life Annuity ------------------------------- #
# ---------------------------------------------------------------------------- #

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

    # Initialize output vector
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

# ---------------------------------------------------------------------------- #
# ------------------------------- Pool Annuity ------------------------------- #
# ---------------------------------------------------------------------------- #

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

    # Initialize output vector
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

# calculate_unit_annuity_due <- function(time, interest, mortality_expected) {

#     # Initiliaze output variable
#     value <- 0

#     # Add discounted count of lives alive at time t
#     discount <- 1
#     for (i in seq(time, length(mortality_expected))) {
#         value <- value + (discount * mortality_expected[i])
#         discount <- discount / (1 + interest)
#     }

#     # Divide sum by size of initial population to get unit annuity-due
#     value <- value/mortality_expected[1]

#     return(value)
# }

# ---------------------------------------------------------------------------- #
# ----------------------------- Reverse Mortgage ----------------------------- #
# ---------------------------------------------------------------------------- #

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
