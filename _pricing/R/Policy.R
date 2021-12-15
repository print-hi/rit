
# using case policy <- create_policy_AP(balance = 500000, expenses = 25500)
create_policy_AP <- function(balance, expenses) {
    pol <- data.frame(name = c("AP"), bal = c(balance), exp = c(expenses))
    return(pol)
}


create_policy_RM <- function(value, age = 60, cost = 0.01, premium = 0.01) {
    pol <- data.frame(name = c("RM"), age = c(age),
                         cost = c(cost), premium = c(premium))
    return(pol)
}

create_policy_LA <- function(benefit, defer) {
    pol <- data.frame(name = c("RM"), benefit = c(benefit), defer = c(defer))
    return(pol)
}

create_policy_VA <- function() {

}


create_policy_PA <- function() {

}


create_policy_CA <- function() {

}



