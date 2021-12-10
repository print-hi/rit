# Initialize scope
setwd("/Users/cocoamilko/Documents/GitHub/retirement-toolkit/_pricing")
library("devtools")

# Temporary helper function, should link to health-state module
get_health_state_3 <- function(age = 17, gender = "F", paths = 1000) {
  health_3 <- as.matrix(read.csv("R/data/health.csv"))
  health_3 <- ifelse(health_3 > 0, -2, health_3)
  colnames(health_3) <- NULL
  rownames(health_3) <- NULL
  return(health_3)
}

get_health_state_5 <- function(age = 17, gender = "F", paths = 1000) {
  health_5 <- as.matrix(read.csv("R/data/health.csv"))
  colnames(health_5) <- NULL
  rownames(health_5) <- NULL
  return(health_5)
}

# Temporary helper function, should link to aggregate-mortality module
get_aggregate_mortality <- function(age = 17, gender = "F", paths = 1000) {
  mortality <- as.matrix(read.csv("R/data/mortality.csv"))
  colnames(mortality) <- NULL
  rownames(mortality) <- NULL
  return(mortality)
}

#' Policy Cashflow Simulator
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name cashflow
#' @param age
#' Initial age of policyholder in years
#' @param gender
#' Gender of policyholder, `gender = "F"` (female), `"M"` (male)
#' @param policy
#' Policy type to simulate:
#' `policy = "AP"` (Account Based Pension),
#' `"RM"` (Reverse Mortgage),
#' `"LA"` (Life Annuity),
#' `"CA"` (Care Annuity),
#' `"PA"` (Pooled Annuity),
#' `"VA"` (Variable Annuity)
#' @param paths
#' Number of paths to simulate (Monte-Carlo method)
#' @return
#' Matrix of cash flow vectors for each simulated path
#' @export cashflow
#' @examples
#' cf <- cashflow(age = 65, gender = "M", policy = "VA", paths = 1000)
cashflow <- function(age = 17, gender = "F", policy = "RM", paths = 1000) {

    # Set state transition + cash flow functions based on input policy
    if (policy == "RM") {
        trans_gen <- get_health_state_3
        cashf_gen <- cf_reverse_mortgage
    } else if (policy == "CA") {
        trans_gen <- get_health_state_5
        cashf_gen <- cf_reverse_mortgage
    } else {
        trans_gen <- get_aggregate_mortality
        cashf_gen <- switch(policy,
                            "AP" = cf_account_based_pension,
                            "LA" = cf_life_annuity,
                            "CA" = cf_care_annuity,
                            "PA" = cf_pooled_annuity,
                            "VA" = cf_variable_annuity)
    }

    # Get matrix of states for paths number of lives
    state <- trans_gen(age, gender, paths)

    # Initialize output matrix
    cf <- matrix(nrow = nrow(state), ncol = ncol(state))

    # Generate cash flows for each state vector
    for (i in seq(1, nrow(state)))
        cf[i, ] <- cashf_gen(state[i, ])

    return(cf)
}