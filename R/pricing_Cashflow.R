# ---------------------------------------------------------------------------- #
# --------------------------- Account Based Pension -------------------------- #
# ---------------------------------------------------------------------------- #

#' Account Based Pension - Cashflow Simulator
#'
#' Simulate cash flows for Account Based Pension
#' @param policy
#' Policy object containing necessary parameters (see create_policy_AP)
#' @param state
#' State vector containing state values for entire duration
#' @param data
#' Data frame containing all variables generated using other modules
#'
#' @return
#' Vector of cashflows for at each time point
cf_account_based_pension <- function(policy, state, data) {

    # Extract relevant policy variables
    balance <- policy$bal
    expense <- policy$exp

    # Initialize output vector
    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1 & i < length(state)) {    # while PH is not dead

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


#' Care Annuity - Cashflow Simulator
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_CA)
#' @param state
#' State vector containing state values for entire duration
#' @param data
#' Data frame containing all variables generated using other modules
#'
#' @return
#' Vector of cashflows for at each time point
cf_care_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    increase <- policy$increase
    benefit <- policy$benefit
    minimum <- policy$min

    # Initialize output vector
    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1 & i < length(state)) {     # while PH is not dead

        # Base + additional benefits from LTC: state[i] = 1 (M), 2 (D), 3(MD)
        if (!state[i]) {
            cf[i] <- benefit[1]
        } else {
            cf[i] <- benefit[1] + benefit[state[i] + 1]
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


#' Life Annuity - Cashflow Simulator
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_LA)
#' @param state
#' State vector containing state values for entire duration
#' @param data
#' Data frame containing all variables generated using other modules
#'
#' @return
#' Vector of cashflows for at each time point
cf_life_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    increase <- policy$increase
    benefit <- policy$benefit
    d <- policy$defer

    # Initialize output vector
    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1 & i < length(state)) {     # while PH is not dead

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
# ------------------------------ Pooled Annuity ------------------------------ #
# ---------------------------------------------------------------------------- #


#' Pooled Annuity - Cashflow Simulator
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_PA)
#' @param state
#' State vector containing state values for entire duration
#' @param data
#' Data frame containing all variables generated using other modules
#'
#' @return
#' Vector of cashflows for at each time point
cf_pooled_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    size <- policy$size
    benefit <- policy$benefit
    interest <- policy$interest

    # Initialize output vector
    cf <- rep(0, times = length(state))

    i <- 1
    while (state[i] != -1 & i < length(state)) {     # while PH is not dead

        # Get benefit if alive
        cf[i] <- benefit

        # Skip rate calculations for final year
        if (i + 1 > length(state)) break

        # Calculate expected and realized survivorship rates
        suv_e <- data$pool_e[i + 1] / data$pool_e[i]
        suv_r <- data$pool_r[i + 1] / data$pool_r[i]

        if (suv_r == 0) break

        # Increase benefit by ratio of rates
        benefit <- benefit * suv_e / suv_r

        # Compound realized + discount expected rates
        benefit <- benefit * (1 + data$stock[i]) / (1 + interest)

        i <- i + 1
    }

    # Calculate value of unit annuity for entire pool
    ax <- rep(0, length(data$pool_e))
    for (i in seq(1, length(data$pool_e))) {
        ax[i] <- (data$pool_e[i]/data$pool_e[1]) * (1 + interest)^(-i)
    }

    # Deduct initial costs from cashflow
    # cf[1] <- cf[1] - policy$benefit * sum(ax)

    return(cf)
}

# For indexed benefits (e.g. inflation)
# benefit <- benefit * (1 + index[i])

# calculate_unit_annuity_due <- function(time, interest, mortality_expected) {

#     # Initialize output variable
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

#' Reverse Mortgage - Cashflow Simulator
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_RM)
#' @param state
#' State vector containing state values for entire duration
#' @param data
#' Data frame containing all variables generated using other modules
#'
#' @return
#' Vector of cashflows for at each time point
cf_reverse_mortgage <- function(policy, state, data) {

    # Extract relevant policy variables
    LVR <- policy$LVR
    cost <- policy$trans_cost
    value <- policy$value
    margin <- policy$margin

    # Initialize output vector
    cf <- rep(0, times = length(state))

    # Get loan amount for policyholder
    loan <- LVR * value
    #cf[1] <- loan

    i <- 1
    while (state[i] == 0 & i < length(state)) {     # while PH is healthy

        # Compound loan value over 1 year period (excluding lending margin)
        loan <- loan * (1 + data$zcp3m[i])

        # Update house value after 1 year period
        value <- value * (1 + data$house[i])

        i <- i + 1
    }

    # Add excess from lending margin to loan
    loan <- loan * exp((i - 1) * margin)

    # Calculate cashflow from sale (includes negative value)
    cf[i] <- max(loan - (1 - cost) * value, 0)

    return(cf)
}

# consider using { state[i] > -1 & state[i] < 3 } for 5 state model

# ---------------------------------------------------------------------------- #
# ----------------------------- Variable Annuity ----------------------------- #
# ---------------------------------------------------------------------------- #


#' Variable Annuity - Cashflow Simulator
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_VA)
#' @param state
#' State vector containing state values for entire duration
#' @param data
#' Data frame containing all variables generated using other modules
#'
#' @return
#' Vector of cashflows for at each time point
cf_variable_annuity <- function(policy, state, data) {

    # Extract relevant policy variables
    value <- policy$value
    contract_length <- policy$length
    withdraw_prop <- policy$prop
    g_fee <- policy$g_fee

    #s_fee <- policy$s_fee # not used in static model

    # Initialize output vector
    cf <- rep(0, times = length(state))

    # Tracks Total amount that can be withdrawn + Value of portfolio account
    total_remaining <- value
    max_withdraw <- value * withdraw_prop

    # Start account value at time 1, as no time 0 cashflow
    account_value <- value * (1 + data$stock[1])

    i <- 2
    while (state[i] != -1 & i < length(state) & i <= contract_length) {

        # Compound account value - expenses for withdrawl guarantee
        account_value <- account_value * (1 + data$stock[i]) * exp(-g_fee)

        # Calculate withdraw limit for current period
        withdraw_limit <- min(max_withdraw, total_remaining)

        # Receive account balance at maturity, otherwise withdrawn maximum
        # amount admissible and update values
        if (i < contract_length) {
            cf[i] <- withdraw_limit
            account_value <- max(account_value - withdraw_limit, 0)
            total_remaining <- max(total_remaining - withdraw_limit, 0)
        } else {
            cf[i] <- account_value
            break
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
