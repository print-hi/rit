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
# ap <- create_policy_AP(balance = 500000, expenses = 25500)

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
create_policy_RM <- function(value, LVR, trans_cost, margin) {
    pol <- data.frame(name = c("RM"), value = c(value), LVR = c(LVR),
                      trans_cost = c(trans_cost), margin = c(margin))
    return(pol)
}
# rm <- create_policy_RM(500000, 60, 0.04, 0.01)

#' Title
#'
#' @param benefit
#' @param defer
#'
#' @return
#' @export
#'
#' @examples
create_policy_LA <- function(benefit, defer, increase = 0, loading) {
    pol <- data.frame(name = c("LA"), benefit = c(benefit), defer = c(defer),
                      increase = c(increase), loading = c(loading))
    return(pol)
}
# la <- create_policy_LA(120000, 5, 0.04, 0.01)

create_policy_CA <- function(benefit, increase, min, loading) {

    if (length(benefit) != length(increase) & length(benefit) != length(min)) {
        print("error")
    } else if (length(benefit) == 2) {
        state <- c("H", "S")
    }
    else if (length(benefit) == 4) {
        state <- c("H", "M", "D", "MD")
    } else {
        print("error")
    }

    pol <- data.frame(name = c("CA"), state = c(state), increase = c(increase),
                      benefit = c(benefit), min = c(min), loading = c(loading))
    return(pol)
}
# benefit <- c(12000, 24000, 24000, 18000)
# increase <- c(0, 0.03, 0.03, 0.03)
# min <- c(10, 0 , 0, 0)
# ca <- create_policy_CA(benefit, increase, min, 0.01)


create_policy_PA <- function() {

}

create_policy_VA <- function() {

}









