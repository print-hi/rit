
# using case
# ap <- create_policy_AP(balance = 500000, expenses = 25500)
# rm <- create_policy_RM(500000, 60, 0.04, 0.01)

#' Title
#'
#' @param balance
#' @param expenses
#'
#' @return
#' @export
#'
#' @examples
create_policy_AP <- function(balance, expenses) {
    pol <- data.frame(name = c("AP"), bal = c(balance), exp = c(expenses))
    return(pol)
}

#' Title
#'
#' @param value
#' @param age
#' @param cost
#' @param margin
#'
#' @return
#' @export
#'
#' @examples
create_policy_RM <- function(value, age, cost, margin) {
    pol <- data.frame(name = c("RM"), value = c(value), age = c(age),
                      cost = c(cost), margin = c(margin))
    return(pol)
}

#' Title
#'
#' @param benefit
#' @param defer
#'
#' @return
#' @export
#'
#' @examples
create_policy_LA <- function(benefit, defer) {
    pol <- data.frame(name = c("RM"), benefit = c(benefit), defer = c(defer),
                      loading = c(loading))
    return(pol)
}

create_policy_VA <- function() {

}


create_policy_PA <- function() {

}


create_policy_CA <- function() {

}



