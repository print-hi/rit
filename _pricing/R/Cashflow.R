#' Policy Cashflow Simulator
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name cashflow
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
#' @return
#' Matrix of cash flow vectors for each simulated path
#' @export cashflow
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
cashflow <- function(policy = "AP", age = 17, sex = "F", seed = 0, n = 1000) {

    # Set cash flow functions based on input policy
    cf_fn <- switch(policy,
                    "AP" = cf_account_based_pension,
                    "RM" = cf_reverse_mortgage,
                    "VA" = cf_variable_annuity,
                    "PA" = cf_pooled_annuity,
                    "CA" = cf_care_annuity,
                    "LA" = cf_life_annuity)

    # Get matrix of states for each path
    if (policy == "RM") {
        state <- get_health_state_3(age, sex, seed, n)
    } else if (policy == "CA") {
        state <- get_health_state_5(age, sex, seed, n)
    } else {
        state <- get_aggregate_mortality(age, sex, seed, n)
    }

    # Get matrix of economic variables for each path
    intrs <- get_interest(age, seed, n)
    infla <- get_inflation(age, seed, n)
    stock <- get_stock_price(age, seed, n)
    house <- get_house_price(age, seed, n)

    # Organise economic inputs into a data.frame for each path
    econ <- list()
    for (i in seq(1, nrow(state))) {
        data <- data.frame(house = house[i, ], infla = infla[i, ],
                           intrs = intrs[i, ], stock = stock[i, ])
        econ <- append(econ, list(data))
    }

    # Initialize output matrix
    cf <- matrix(nrow = nrow(state), ncol = ncol(state))

    # Generate cash flows for each state vector
    for (i in seq(1, nrow(state))) cf[i, ] <- cf_fn(state[i, ], econ[[i]])

    return(cf)
}

###############################################################################
###### PLACEHOLDER FUNCTIONS

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

# Temporary helper function, should link to mortality module
get_aggregate_mortality <- function(age = 17, sex = "F", seed = 0, n = 1000) {
  mortality <- as.matrix(read.csv("R/data/mortality.csv"))
  colnames(mortality) <- NULL
  rownames(mortality) <- NULL
  return(mortality)
}

# Temporary helper function, should link to economic module
get_interest <- function(age = 17, seed = 0, n = 1000) {
  interest <- as.matrix(read.csv("R/data/interest.csv"))
  colnames(interest) <- NULL
  rownames(interest) <- NULL
  return(interest)
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
