#' Simulate Individual Life Path
#'
#' Simulates the life path of an individual using 1-year death probabilities
#'
#' @param init_age
#' the initial age of the path
#' @param sex
#' character string denoting the gender of individuals, "F" for female and "M" for male
#' @param death_probs
#' a vector of 1-year death probabilities. If not supplied, an M7 age-period-cohort
#' model will be fitted on \code{mortality_AUS_data} to produce forecasted
#' death probabilities for a cohort starting at init_age in 2022
#' @param closure_age
#' maximum life span
#' @param n_sim
#' integer (default 10000) denoting number of path simulations, which is equivalent
#' to the number of people in the simulation
#'
#' @return
#' a matrix where each row represents an individual's dead (-1) or alive (0) status
#' across the years
#'
#' @export
#'
#' @examples
#'
simulate_path_mortality <- function(init_age, sex = "F", death_probs = NULL, closure_age = 130, n_sim = 10000) {

# Flagging errors ---------------------------------------------------------

    # init_age
    if (init_age < 55 | init_age > closure_age) {
        stop("initial age must be between 55 and the maximum age")
    }

    if (init_age != floor(init_age)) {
        stop("initial age must be an integer")
    }

    # sex
    if (sex != "F" & sex != "M") {
        stop("sex must be 'F' or 'M'")
    }

    # death_probs
    if (!is.null(death_probs)) {
        if (!is.vector(death_probs) | !is.numeric(death_probs)) {
            stop("death probabilities must be a numeric vector")
        }

        if (any(death_probs < 0, na.rm = T) | any(death_probs > 1, na.rm = T) ) {
            stop("1-yr death probabilities must be between 0 and 1")
        }

        if (length(death_probs) != closure_age - init_age + 1) {
            stop("number of death probabilities does not correspond to the given initial and max age")
        }
        if (!dplyr::near(tail(death_probs, 1), 1)) {
            stop("1-yr death probability at the maximum age must be 1")
        }
    }
    # closure_age
    if (closure_age < 90 | closure_age != floor(closure_age)) {
        stop("maximum age must be an integer greater than 89")
    }

    # n_sim
    if (n_sim <= 0 | n_sim != floor(n_sim)) {
        stop('number of simulations must be a positive integer')
    }

# Implementation ----------------------------------------------------------

    # Generating default death probabilities for males and females if required
    if (is.null(death_probs)) {
        death_probs <- generate_default_qx(init_age, sex, closure_age)
    }

    # create empty matrix of simulated paths
    sim_path <- matrix(NA, nrow = n_sim, ncol = closure_age - init_age+2)
    # initialise as health
    sim_path[,1] <- 0
    colnames(sim_path) <- as.character(init_age:(closure_age + 1))

    # qx should be vector of length (closure_age - init_age + 1)
    for (i in 2:ncol(sim_path)) {
        # Simulate TRUE/FALSE vector indicating if individual has died
        # during this transition
        sim_death <- (runif(n_sim) <= death_probs[i - 1])
        # Encoding to 0 for alive and -1 for dead
        sim_path[, i] <- -as.numeric(sim_death)
        # transitions cannot occur for dead individuals
        sim_path[sim_path[, i-1] == -1, i] <- -1
    }
    return(sim_path)

}





