#' Policy Cashflow Simulator
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name simulate_cf
#' @param policy
#' Policy type to simulate:
#' `policy = "AP"` (Account Based Pension),
#' `"RM"` (Reverse Mortgage),
#' `"LA"` (Life Annuity),
#' `"CA"` (Care Annuity),
#' `"PA"` (Pooled Annuity),
#' `"VA"` (Variable Annuity)
#' @param age
#' Initial age of policyholder in years
#' @param sex
#' sex of policyholder, `sex = "F"` (female), `"M"` (male)
#' @param seed
#' Seed for random generator
#' @param n
#' Number of paths to simulate (Monte-Carlo method)
#' @param value
#' Amount of
#' @param withdrawl
#' ABP only, amount withdrawn in first year from account i.e. yearly expense
#' @param loading
#' Number of paths to simulate (Monte-Carlo method)
#' @param gmwb
#' Number of paths to simulate (Monte-Carlo method)
#' @return
#' Matrix of cash flow vectors for each simulated path
#' @export cashflow
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
simulate_cf <- function(policy, age = 17, sex = "F", seed = 0, n = 1000) {

    # Set cash flow function based on input policy
    cf_func <- switch(policy$name[1], "AP" = cf_account_based_pension,
                                      "RM" = cf_reverse_mortgage,
                                      "VA" = cf_variable_annuity,
                                      "PA" = cf_pooled_annuity,
                                      "CA" = cf_care_annuity,
                                      "LA" = cf_life_annuity)

    # Get matrix of states for each path
    if (policy$name[1] == "CA") {
        if (nrow(policy) == 2) {
            state <- get_health_state_3(age, sex, seed, n)
        } else if (nrow(policy) == 4) {
            state <- get_health_state_5(age, sex, seed, n)
        } else {
            print("error")
        }
    } else if (policy$name[1] == "RM") {
        state <- get_health_state_3(age, sex, seed, n)
    } else {
        state <- get_aggregate_mortality(age, sex, seed, n)
    }

    # Get matrix of economic variables for each path
    data <- get_policy_scenario(policy)

    # Get dimensions of state matrix
    nrow_s = nrow(state); ncol_s = ncol(state)

    # Initialize output matrix
    cf <- matrix(nrow = nrow_s, ncol = ncol_s)

    # Generate cash flows for each state vector
    for (i in seq(1, nrow_s)) cf[i,] <- cf_func(policy, state[i,], data[[i]])

    return(cf)
}


###############################################################################
###### POLICY SCENARIO FUNCTION

get_policy_scenario <- function(policy) {

    if (policy$name[1] == "AP") {

        # Get all relevant economic variables
        infla <- get_inflation(age, seed, n)
        stock <- get_stock_price(age, seed, n)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, nrow(state))) {
            temp <- data.frame(infla = infla[i, ], stock = stock[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "CA" | policy$name[1] == "LA") {

        # Get all relevant economic variables
        infla <- get_inflation(age, seed, n)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, nrow(state))) {
            temp <- data.frame(infla = infla[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "PA") {

        # Get all relevant health variables for pool
        pool_r <- get_pool_realised(age, seed, n)
        pool_e <- get_pool_expected(age, seed, n)

        # Get all relevant economic variables
        stock <- get_stock_price(age, seed, n)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, nrow(state))) {
            temp <- data.frame(pool_r = pool_r[i, ], pool_e = pool_e,
                               stock = stock[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "RM") {

        # Get all relevant economic variables
        rfree <- get_risk_free(age, seed, n)
        house <- get_house_price(age, seed, n)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, nrow(state))) {
            temp <- data.frame(house = house[i, ], rfree = rfree[i, ])
            data <- append(data, list(temp))
        }

    } else if (policy$name[1] == "VA") {

        # Get all relevant economic / health variables
        intrs <- get_interest(age, seed, n)
        infla <- get_inflation(age, seed, n)
        stock <- get_stock_price(age, seed, n)
        house <- get_house_price(age, seed, n)

        # Organise economic inputs into a data.frame for each path
        data <- list()
        for (i in seq(1, nrow(state))) {
            temp <- data.frame(house = house[i, ], infla = infla[i, ],
                               intrs = intrs[i, ], stock = stock[i, ])
            data <- append(data, list(temp))
        }

    } else {
        print("error")
    }

    return(data)

}


###############################################################################
###### PLACEHOLDER FUNCTIONS

# ------------------------------------------------------------------------
# ---- Health State Module

# Temporary helper function, should link to health-state module
get_health_state_3 <- function(age = 17, sex = "F", seed = 0, n = 1000) {
    health_3 <- as.matrix(read.csv("R/data/health.csv"))
    health_3 <- ifelse(health_3 > 0, -2, health_3)
    colnames(health_3) <- NULL
    rownames(health_3) <- NULL
    return(health_3)
}

# Temporary helper function, should link to health-state module
get_health_state_5 <- function(age = 17, sex = "F", seed = 0, n = 1000) {
    health_5 <- as.matrix(read.csv("R/data/health.csv"))
    colnames(health_5) <- NULL
    rownames(health_5) <- NULL
    return(health_5)
}

# ------------------------------------------------------------------------
# ---- Aggregate Mortality Module

# Temporary helper function, should link to mortality module
get_aggregate_mortality <- function(age = 17, sex = "F", seed = 0, n = 1000) {
    mortality <- as.matrix(read.csv("R/data/mortality.csv"))
    colnames(mortality) <- NULL
    rownames(mortality) <- NULL
    return(mortality)
}

# Temporary helper function, should link to mortality module
get_pool_realised <- function(age = 17, sex = "F", seed = 0, n = 1000) {
    pool <- as.matrix(read.csv("R/data/pool.csv"))
    colnames(pool) <- NULL
    rownames(pool) <- NULL
    return(pool)
}

# Temporary helper function, should link to mortality module
get_pool_expected <- function(age = 17, sex = "F", seed = 0, n = 1000) {
    pool <- as.matrix(read.csv("R/data/pool-exp.csv"))
    colnames(pool) <- NULL
    rownames(pool) <- NULL
    return(pool)
}

# ------------------------------------------------------------------------
# ---- Economic Scenario Generator Module

# Temporary helper function, should link to economic module
get_interest <- function(age = 17, seed = 0, n = 1000) {
    interest <- as.matrix(read.csv("R/data/interest.csv"))
    colnames(interest) <- NULL
    rownames(interest) <- NULL
    return(interest)
}

# Temporary helper function, should link to economic module
get_risk_free <- function(age = 17, seed = 0, n = 1000) {
    risk_free <- as.matrix(read.csv("R/data/interest.csv"))
    colnames(risk_free) <- NULL
    rownames(risk_free) <- NULL
    return(risk_free)
}

# Temporary helper function, should link to economic module
get_inflation <- function(age = 17, seed = 0, n = 1000) {
    inflation <- as.matrix(read.csv("R/data/inflation.csv"))
    colnames(inflation) <- NULL
    rownames(inflation) <- NULL
    return(inflation)
}

# Temporary helper function, should link to economic module
get_house_price <- function(age = 17, seed = 0, n = 1000) {
    house <- as.matrix(read.csv("R/data/house.csv"))
    colnames(house) <- NULL
    rownames(house) <- NULL
    return(house)
}

# Temporary helper function, should link to economic module
get_stock_price <- function(age = 17, seed = 0, n = 1000) {
    stock <- as.matrix(read.csv("R/data/stock.csv"))
    colnames(stock) <- NULL
    rownames(stock) <- NULL
    return(stock)
}
