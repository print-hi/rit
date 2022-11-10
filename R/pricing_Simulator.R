###############################################################################
###### MAIN SIMULATION WRAPPER

#' Policy Cashflow Simulator
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#'
#' @name simulate_cf
#' @param policy
#' Policy type to simulate:
#' `policy object created using a create_policy function
#' @param age
#' Initial age of policyholder in years
#' @param sex
#' sex of policyholder, `sex = "F"` (female), `"M"` (male)
#' @param seed
#' Seed for random generator
#' @param n
#' Number of paths to simulate (Monte-Carlo method)
#' @param state
#' Simulated state matrix via Health-State / Aggregate Mortality
#' @param econ_var
#' Simulated economic variables via Economic Scenario Generator
#' @return
#' Matrix of cash flow vectors for each simulated path
#' @export simulate_cf
#' @examples
#' ap <- create_policy_AP(400000, 60000)
#' cf <- simulate_cf(policy = ap, age = 65, sex = "M", n = 1000)
simulate_cf <- function(policy, age = 65, sex = "F", seed = 0, n = 100, state = NULL, econ_var = NULL) {

    # Set cash flow function based on input policy
    cf_func <- switch(policy$name[1], "AP" = cf_account_based_pension,
                                      "RM" = cf_reverse_mortgage,
                                      "VA" = cf_variable_annuity,
                                      "PA" = cf_pooled_annuity,
                                      "CA" = cf_care_annuity,
                                      "LA" = cf_life_annuity)
    # Get matrix of states for each path
    if (is.null(state)) {
        state <- get_state_simulation(policy, age, sex, seed, n)
    }

    if (nrow(state) != n) {
        stop("Error: State matrix does not fit number of paths requested")
    } else if (!is.null(econ_var) && ncol(state) > ncol(econ_var)) {
        stop("Error: Duration of economic simulation is too short")
    }

    # Ensures that state <-> data has 1:1 match for each path at each time
    # DISABLED WHILE USING TEMP DATA, needs MAX_AGE defined !!!
    #if (ncol(state) != nrow(data))  stop("Error fetching policy data")

    if (is.null(econ_var)) {
        econ_var <- esg_var_simulator(ncol(state), n, frequency = 'year', return_sdf = TRUE)
    }

    # Get matrix of economic variables for each path
    data <- get_policy_scenario(policy, age, sex, seed, n, period, econ_var)

    # Initialize output matrix
    cf <- matrix(nrow = n, ncol = ncol(state))

    # Generate cash flows for each state vector
    for (i in seq(1, n)) {
        cf[i,] <- cf_func(policy, state[i,], data[[i]])
    }

    # Round cashflows to cents
    cf <- round(cf, 2)

    result <- list(cf = cf, sdf = unname(econ_var$discount_factors))

    return(result)
}


###############################################################################
###### POLICY SCENARIO FUNCTION

#' Scenario Generator
#'
#' Generates and encapsulates scenario data for a given policy
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_ )
#' @param age
#' Initial age of policyholder in years
#' @param sex
#' Sex of policyholder
#' @param seed
#' Seed for random generator
#' @param n
#' Number of paths to simulate (Monte-Carlo method)
#' @param period
#' Number of periods to simulate
#' @param econ_var
#' Simulated economic variables via Economic Scenario Generator
#' @return
#' Matrix of cash flow vectors for each simulated path
#'
#' @return
#' Data frame containing all variables generated using other modules
get_policy_scenario <- function(policy, age, sex, seed, n, period, econ_var) {

    var_sim <- econ_var

    if (policy$name[1] == "AP") {

        # Get all relevant economic variables
        infla <- get_inflation_rate(var_sim)
        stock <- get_stock_return(var_sim)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, n)) {
            temp <- data.frame(infla = infla[i, ],
                               stock = stock[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "CA" | policy$name[1] == "LA") {

        # Get all relevant economic variables
        infla <- get_inflation_rate(var_sim)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, n)) {
            temp <- data.frame(infla = infla[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "PA") {

        # Get all relevant health variables for pool
        pool_r <- get_pool_realised(age, sex, seed, n, policy$size)
        pool_e <- get_pool_expected(age, sex, seed, policy$size)

        # Get all relevant economic variables
        stock <- get_stock_return(var_sim)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, n)) {
            temp <- data.frame(pool_r = pool_r[i, ],
                               pool_e = pool_e,
                               stock = stock[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "RM") {

        # Get all relevant economic variables
        zcp3m <- get_zcp3m_yield(var_sim)
        house <- get_house_return(var_sim)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, n)) {
            temp <- data.frame(house = house[i, ],
                               zcp3m = zcp3m[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "VA") {

        # Get all relevant economic / health variables
        stock <- get_stock_return(var_sim)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, n)) {
            temp <- data.frame(stock = stock[i, ])
            data <- append(data, list(temp))
        }

    } else {
        stop("Error: invalid policy object")
    }

    return(data)

}

get_state_simulation <- function(policy, age, sex, seed, n) {
    if (policy$name[1] == "CA") {
        if (nrow(policy) == 2) {
            probs <- get_trans_probs(3, 'S', rit::US_HRS, age, sex == 'F')
        } else if (nrow(policy) == 4) {
            probs <- get_trans_probs(5, 'S', rit::US_HRS_5, age, sex == 'F')
        } else {
            stop("Error: CA policy object needs to have 2 or 4 rows")
        }
        return(simulate_health_state_paths(probs, age, cohort = n))
    } else if (policy$name[1] == "RM") {
        probs <- get_trans_probs(3, 'S', rit::US_HRS, age, sex == 'F')
        return(simulate_health_state_paths(probs, age, cohort = n))
    } else {
        return(get_aggregate_mortality(age, sex, seed, n))
    }
}


###############################################################################
###### PLACEHOLDER FUNCTIONS

# ------------------------------------------------------------------------
# ---- Health State Module

get_health_state_3 <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    trans_probs <-  get_trans_probs(3, 'T', rit::US_HRS, age, (sex == "F"), year = 2022)
    return(simulate_health_state_paths(trans_probs, age, 0, n))
}

# TODO
get_health_state_5 <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    trans_probs <-  get_trans_probs(5, 'T', rit::US_HRS_5, age, (sex == "F"), year = 2012, wave_index = 8, latent = 0)
    return(simulate_health_state_paths(trans_probs, age, 0, n))
}

# ------------------------------------------------------------------------
# ---- Aggregate Mortality Module

# Temporary helper function, should link to mortality module
get_aggregate_mortality <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    utils::capture.output(suppressWarnings(
        mortality <- sim_indiv_path(age, sex, death_probs = NULL, closure_age = 130, n)
    ))
    return(mortality)
}

# Temporary helper function, should link to mortality module
get_pool_realised <- function(age = 65, sex = "F", seed = 0, n = 1000, cohort = 1000) {
    utils::capture.output(suppressWarnings(
        pool <- sim_cohort_path_realised(age, sex, death_probs = NULL, closure_age = 130, cohort, n)
    ))
    return(pool)
}

# Temporary helper function, should link to mortality module
get_pool_expected <- function(age = 65, sex = "F", seed = 0, cohort = 1000) {
    utils::capture.output(suppressWarnings(
        pool <- sim_cohort_path_expected(age, sex, death_probs = NULL, closure_age = 130, cohort)
    ))
    return(pool)
}

# ------------------------------------------------------------------------
# ---- Economic Scenario Generator Module

get_perc_change <- function(df) {
    result <- df
    for (i in seq(1, ncol(df) - 1)) {
        result[,i] <- (df[,i + 1]/df[,i]) - 1
    }
    result[, ncol(df)] <- result[, ncol(df) - 1] # to-do; generate extra year to truncate
    return(result)
}

get_zcp3m_yield <- function(var_sim) {
    return((unname(var_sim$zcp3m_yield)))
}

get_inflation_rate <- function(var_sim) {
    cpi <- (unname(var_sim$CPI))
    return(get_perc_change(cpi))
}

get_house_return <- function(var_sim) {
    home_index <- (unname(var_sim$home_index))
    return(get_perc_change(home_index))
}

get_stock_return <- function(var_sim) {
    asx <- (unname(var_sim$ASX200))
    return(get_perc_change(asx))
}
