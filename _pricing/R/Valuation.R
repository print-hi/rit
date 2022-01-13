# ------------------------------------------------------------------------
# ---- Economic Scenario Generator Module

# Temporary helper function, should link to economic module
get_sdf <- function(n = 1000, period = 100) {
    interest <- as.matrix(read.csv("R/data/sdf.csv", header = FALSE))
    colnames(interest) <- NULL
    rownames(interest) <- NULL
    return(interest)
}


#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name value_cf
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param ret_paths
#' If ret_paths = TRUE, vector of prices for each path is returned
#' @return
#' Value of policy
#' @export value_cf
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
value_cf <- function(cashflows, ret_paths = FALSE) {

    # For debugging purposes
    n_paths <- 10000
    periods <- 100

    # Extract matrix dimensions
    n_paths <- nrow(cashflows)
    periods <- ncol(cashflows)

    # Get Stochastic Discount Factors from ESG module and calculate
    # cumulative product of factors
    sdf <- get_sdf(n_paths, periods)
    cmsdf <- rowCumprods((1/sdf))

    # Calculate discounted value of cashflows for each path
    value <- rowSums(cashflows * cmsdf)

    # Return vector of cashflows if requested via params
    if (ret_paths) return(value)

    # Otherwise, return expected value
    return(mean(value))

}


#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name plot_cf
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param convergence
#' If convergence = TRUE, plots convergence of price
#' @return
#' Value of policy
#' @export plot_cf
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
plot_cf <- function(cashflows, convergence = FALSE) {

    data <- value_cf(cashflows, TRUE)

    plot(data)

}
