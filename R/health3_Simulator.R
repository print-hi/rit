# simulation functions

#' Simulate cohort life path
#'
#' Simulates the path each life takes in an initial cohort using transition probabilities
#'
#' @param trans_probs
#' a list of transition probability matrices, preferably generated from \code{health3_get_trans_probs}.
#' @param init_age
#' integer between 65 and 110 denoting current age. This has to the be same as the initial
#' age used in the generation of transition probability matrices.
#' @param init_state
#' 0 for healthy, 1 for disabled
#' @param cohort
#' integer (default 10000) denoting number of people in the simulation
#'
#' @return
#' a matrix where each row represents a new individual, and the columns represent
#' the individual's movement through each state.
#'
#' -1 (death) is absorbing, so if an individual enters that state, the rest of the row will be -1.
#'
#'
#' @examples example
health3_simulate_paths <- function(trans_probs, init_age, init_state, cohort) {
  # screening for errors
  if (init_state != 0 & init_state != 1) {
    stop('invalid state, 0 for healthy and 1 for disabled')
  }

  if (init_age<65 | init_age>110) {
    stop('invalid age')
  }

  if (as.integer(init_age) != init_age) {
    stop('initial age must be an integer')
  }

  if (length(trans_probs) != 111 - init_age) {
    stop('initial age does not correspond to the number of transition probability matrices')
  }

  if (cohort < 0) {
    stop('cohort needs to be a positive integer')
  }

  if (as.integer(cohort) != cohort) {
    stop('cohort needs to be an integer')
  }

  # create empty matrix to contain simulated population
  simulated_pop <- matrix(0, nrow = cohort, ncol = 110-init_age+2)

  # initialise all individuals
  simulated_pop[, 1] <- init_state

  for (i in 2:ncol(simulated_pop)) {
    simulated_pop[simulated_pop[,i-1] == 0, i] <- sample(c(0, 1, -1),
                                                        sum(simulated_pop[, i-1] == 0),
                                                        replace = TRUE,
                                                        prob = trans_probs[[i-1]][1, ])
    simulated_pop[simulated_pop[,i-1] == 1, i] <- sample(c(0, 1, -1),
                                                        sum(simulated_pop[, i-1] == 1),
                                                        replace = TRUE,
                                                        prob = trans_probs[[i-1]][2, ])
    simulated_pop[simulated_pop[,i-1] == -1, i] <- -1
  }
  return(simulated_pop)
}
