#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name value_cf
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @return
#' Value of policy
#' @export value_cf
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
value_cf <- function(cashflows) {

}

#' Policy Valuation
#'
#' Simulate cash flows using Monte-Carlo methods for various policies and price
#' policy
#' @name policy_pricing
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
#' @export policy_pricing
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
policy_pricing <- function(policy, age = 17, sex = "F", seed = 0, n = 1000) {

}
