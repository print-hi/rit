#' Simulate Cohort Life Path
#'
#' Simulates the path each life takes in an initial cohort using 1-year death probabilities
#'
#' @param init_age
#' the initial age of the path
#' @param death_probs
#' a vector of 1-year death probabilities
#' @param closure_age
#' maximum life span
#' @param cohort
#' integer (default 10000) denoting number of people in the simulation
#'
#' @return
#' a matrix where each row represents a new individual, and the columns represent
#' whether the individual is dead (-1) or alive (0)
#'
#' @export
#'
#' @examples
#'
simulate_path_mortality <- function(init_age, death_probs, closure_age = 130, cohort = 10000) {

# Flagging errors ---------------------------------------------------------

    # init_age
    if (init_age < 0 | init_age > closure_age) {
        stop("initial age must be between 0 and the maximum age")
    }

    if (init_age != floor(init_age)) {
        stop("initial age must be an integer")
    }

    # death_probs
    if (!is.vector(death_probs) | !is.numeric(death_probs)) {
        stop("death probabilities must be a numeric vector")
    }

    if (any(death_probs < 0, na.rm = T) | any(death_probs > 1, na.rm = T) ) {
        stop("1-yr death probabilities must be between 0 and 1")
    }

    if (length(death_probs) != closure_age - init_age + 1) {
        stop("number of death probabilities does not correspond to the given initial and max age")
    }

    # cohort
    if (cohort < 0 | cohort != floor(cohort)) {
        stop('cohort must be a positive integer')
    }

# Implementation ----------------------------------------------------------

    # create empty matrix of simulated population
    sim_pop <- matrix(0, nrow = cohort, ncol = closure_age - init_age+2)

    # qx should be vector of length (closure_age - init_age + 1)
    for (i in 2:ncol(sim_pop)) {
        # Simulate TRUE/FALSE vector indicating if individual has died
        # during this transition
        sim_death <- (runif(cohort) <= death_probs[i - 1])
        # Encoding to 0 for alive and -1 for dead
        sim_pop[, i] <- -as.numeric(sim_death)
        # transitions cannot occur for dead individuals
        sim_pop[sim_pop[, i-1] == -1, i] <- -1
    }
    return(sim_pop)

}

