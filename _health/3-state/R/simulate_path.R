# simulation functions

#' Simulate cohort life path
#'
#' Simulates the path each life takes in an initial cohort using transition probabilities
#'
#' @param init_age
#' integer between 64 and 110 denoting current age
#' @param init_state
#' 1 for healthy, 2 for disabled
#' @param cohort
#' integer (default 10000) denoting number of people in the simulation
#' @param trans_probs
#' a list of transition probability matrices, preferably generated from \code{\link[tshm]{get_trans_probs}}.
#'
#' @return
#' a matrix where each row represents a new individual, and the columns represent
#' the individual's movement through each state.
#'
#' 3 (death) is absorbing, so if an individual enters that state, the rest of the row will be 3.
#'
#' @export
#'
#' @examples
simulate_path <- function(init_age, init_state, trans_probs, cohort = 10000) {
  # screening for errors
  if (init_state != 1 & init_state != 2) {
    return('Please enter a valid initial state: 1 for healthy, 2 for disabled.')
  }

  if (init_age<65 | init_age>110) {
    return('Error: init_age outside bounds of allowable age values')
  }

  # create empty matrix to contain simulated population
  simulated_pop <- matrix(0, nrow = cohort, ncol = 110-init_age+1)

  # initialise all individuals
  simulated_pop[, 1] <- init_state

  for (i in 2:ncol(simulated_pop)) {
    simulated_pop[simulated_pop[,i-1] == 1, i] <- sample(c(1, 2, 3),
                                                        sum(simulated_pop[, i-1] == 1),
                                                        replace = TRUE,
                                                        prob = trans_probs[[i-1]][1, ])
    simulated_pop[simulated_pop[,i-1] == 2, i] <- sample(c(1, 2, 3),
                                                        sum(simulated_pop[, i-1] == 2),
                                                        replace = TRUE,
                                                        prob = trans_probs[[i-1]][2, ])
    simulated_pop[simulated_pop[,i-1] == 3, i] <- 3
  }
  return(simulated_pop)
}
