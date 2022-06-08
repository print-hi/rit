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
#' cf <- cashflow(policy = ap, age = 65, sex = "M", n = 1000)
simulate_cf <- function(policy, age = 17, sex = "F", seed = 0, n = 100, state = NULL, econ_var = NULL) {

    # Set cash flow function based on input policy
    cf_func <- switch(policy$name[1], "AP" = cf_account_based_pension,
                                      "RM" = cf_reverse_mortgage,
                                      "VA" = cf_variable_annuity,
                                      "PA" = cf_pooled_annuity,
                                      "CA" = cf_care_annuity,
                                      "LA" = cf_life_annuity)

    if (is.null(state)) {
        # Get matrix of states for each path
        if (policy$name[1] == "CA") {
            if (nrow(policy) == 2) {
                state <- get_health_state_3(age, sex, seed, n)
            } else if (nrow(policy) == 4) {
                state <- get_health_state_5(age, sex, seed, n)
            } else {
                stop("Error: CA policy object needs to have 2 or 4 rows")
            }
        } else if (policy$name[1] == "RM") {
            state <- get_health_state_3(age, sex, seed, n)
        } else {
            state <- get_aggregate_mortality(age, sex, seed, n)
        }
    }

    if (nrow(state) != n) {
        stop("Error: State matrix does not fit number of paths requested")
    } else if (!is.null(econ_var) && ncol(state) > ncol(econ_var)) {
        stop("Error: Duration of economic simulation is too short")
    }

    # Ensures that state <-> data has 1:1 match for each path at each time
    # DISABLED WHILE USING TEMP DATA, needs MAX_AGE defined !!!
    #if (ncol(state) != nrow(data))  stop("Error fetching policy data")
    period <- ncol(state) + 1
    if (is.null(econ_var)) {
        econ_var <- get_var_simulations(period, n, frequency = 'year', return_sdf = TRUE)
    }

    # Get matrix of economic variables for each path
    data <- get_policy_scenario(policy, age, seed, n, period, econ_var)

    # Initialize output matrix
    cf <- matrix(nrow = n, ncol = ncol(state))

    # Generate cash flows for each state vector
    for (i in seq(1, n)) cf[i,] <- cf_func(policy, state[i,], data[[i]])

    result <- list(cf = cf, sdf = t(unname(econ_var$discount_factors)))

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
#' @param seed
#' Seed for random generator
#' @param n
#' Number of paths to simulate (Monte-Carlo method)
#' @param econ_var
#' Simulated economic variables via Economic Scenario Generator
#' @return
#' Matrix of cash flow vectors for each simulated path
#'
#' @return
#' Data frame containing all variables generated using other modules
get_policy_scenario <- function(policy, age, seed, n, period, econ_var) {

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
        pool_r <- get_pool_realised(age, seed, n)
        pool_e <- get_pool_expected(age, seed, n)

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


###############################################################################
###### PLACEHOLDER FUNCTIONS

# ------------------------------------------------------------------------
# ---- Health State Module

get_health_state_3 <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    trans_probs <- get_trans_probs('T', US_HRS, age, (sex == "F"), 2021)
    return(simulate_path(age, 0, trans_probs, n))
}

# TODO
get_health_state_5 <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    return(simulate_individual_path_5(age, 0, params_5_frailty, (sex == "F"), 8, n, 3))
}

# ------------------------------------------------------------------------
# ---- Aggregate Mortality Module

# Temporary helper function, should link to mortality module
get_aggregate_mortality <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    mortality <- as.matrix(read.csv("lib/_pricing/R/data/mortality.csv", header = FALSE))
    colnames(mortality) <- NULL
    rownames(mortality) <- NULL
    return(mortality)
}

# Temporary helper function, should link to mortality module
get_pool_realised <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    pool <- as.matrix(read.csv("lib/_pricing/R/data/pool.csv", header = FALSE))
    colnames(pool) <- NULL
    rownames(pool) <- NULL
    return(pool)
}

# Temporary helper function, should link to mortality module
get_pool_expected <- function(age = 65, sex = "F", seed = 0, n = 1000) {
    pool <- as.matrix(read.csv("lib/_pricing/R/data/pool-exp.csv", header = FALSE))
    colnames(pool) <- NULL
    rownames(pool) <- NULL
    return(pool)
}

# ------------------------------------------------------------------------
# ---- Economic Scenario Generator Module

get_zcp3m_yield <- function(var_sim) {
    zcp3m <- var_sim$zcp3m_yield
    return(t(unname(zcp3m)))
}

get_perc_change <- function(df) {
    result <- df
    for (i in seq(1, ncol(df) - 1)) {
        result[,i] <- (df[,i + 1]/df[,i]) - 1
    }
    result[, ncol(df)] <- result[, ncol(df) - 1] # to-do; generate extra year to truncate
    return(result)
}

get_inflation_rate <- function(var_sim) {
    infla <- get_perc_change(var_sim$CPI)
    return(t(unname(infla)))
}

get_house_return <- function(var_sim) {
    house <- get_perc_change(var_sim$home_index)
    return(t(unname(house)))
}

get_stock_return <- function(var_sim) {
    stock <- get_perc_change(var_sim$ASX200)
    return(t(unname(stock)))
}
