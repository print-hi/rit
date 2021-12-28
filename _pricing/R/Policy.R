# ---------------------------------------------------------------------------- #
# --------------------------- Account Based Pension -------------------------- #
# ---------------------------------------------------------------------------- #

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

# ---------------------------------------------------------------------------- #
# ------------------------------- Care Annuity ------------------------------- #
# ---------------------------------------------------------------------------- #

#' Title
#'
#' @param benefit
#' @param increase
#' @param min
#' @param loading
#'
#' @return
#' @export
#'
#' @examples
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

# ---------------------------------------------------------------------------- #
# ------------------------------- Life Annuity ------------------------------- #
# ---------------------------------------------------------------------------- #

#' Title
#'
#' @param benefit
#' @param defer
#' @param increase
#' @param loading
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

# ---------------------------------------------------------------------------- #
# ------------------------------ Pooled Annuity ------------------------------ #
# ---------------------------------------------------------------------------- #

#' Title
#'
#' @param benefit
#' @param size
#' @param interest
#' @param loading
#'
#' @return
#' @export
#'
#' @examples
create_policy_PA <- function(benefit, size, interest, loading) {
    pol <- data.frame(name = c("PA"), benefit = c(benefit), size = c(size),
                      interest = c(interest), loading = c(loading))
    return(pol)
}

# ---------------------------------------------------------------------------- #
# ----------------------------- Reverse Mortgage ----------------------------- #
# ---------------------------------------------------------------------------- #

#' Title
#'
#' @param value
#' @param margin
#' @param LVR
#' @param trans_cost
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

# ---------------------------------------------------------------------------- #
# ----------------------------- Variable Annuity ----------------------------- #
# ---------------------------------------------------------------------------- #

#' Title
#'
#' @param value
#' @param prop
#' @param step_time
#' @param step_size
#' @param s_fee
#' @param g_fee
#'
#' @return
#' @export
#'
#' @examples
create_policy_VA <- function(value, length, prop, s_fee, g_fee) {
    pol <- data.frame(name = c("RM"), value = c(value), length = c(length),
                      prop = c(prop),  s_fee = c(s_fee), g_fee = c(g_fee))
    return(pol)
}

# create_policy_VA <- function(value, length, prop, s_fee, g_fee, step_time,
#                              step_size) {
#     pol <- data.frame(name = c("RM"), value = c(value), length = c(length),
#                       prop = c(prop),  s_fee = c(s_fee), g_fee = c(g_fee),
#                       step_time = c(step_time), step_size = c(step_size))
#     return(pol)
# }
