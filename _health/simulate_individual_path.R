simulate_individual_path <- function(init_age, init_state, trans_probs, cohort = 10000) {
  # init_state 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead
  
  # create empty matrix to contain simulated population
  simulated_pop <- matrix(0, nrow = cohort, ncol = 110-init_age+2)
  
  # initialise all individuals
  simulated_pop[, 1] <- init_state
  
  for (i in 2:ncol(simulated_pop)) {
    simulated_pop[simulated_pop[,i-1] == 0, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 0),
                                                         replace = TRUE,
                                                         prob = trans_probs[[i-1]][1, ])
    
    
    simulated_pop[simulated_pop[,i-1] == 1, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 1),
                                                         replace = TRUE,
                                                         prob = trans_probs[[i-1]][2, ])
    
    simulated_pop[simulated_pop[,i-1] == 2, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 2),
                                                         replace = TRUE,
                                                         prob = trans_probs[[i-1]][3, ])
    
    
    simulated_pop[simulated_pop[,i-1] == 3, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 3),
                                                         replace = TRUE,
                                                         prob = trans_probs[[i-1]][4, ])
    
    simulated_pop[simulated_pop[,i-1] == -1, i] <- -1
  }
  return(simulated_pop)
}

# example to use
simulated_path=simulate_individual_path(init_age=65, init_state=0, trans_probs=trans_prob_matrix, cohort = 10000)