# simulation functions

simulate_path <- function(init_age, init_state, cohort = 10000, trans_probs) {
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
    simulated_pop[simulated_pop[,i-1] == 1, ] <- sample(c(1, 2, 3),
                                                        sum(simulated_pop[, i-1] == 1),
                                                        replace = TRUE,
                                                        prob = trans_probs[[i-1]][1, ])
    simulated_pop[simulated_pop[,i-1] == 2, ] <- sample(c(1, 2, 3),
                                                        sum(simulated_pop[, i-1] == 2),
                                                        replace = TRUE,
                                                        prob = trans_probs[[i-1]][2, ])
  }
}
