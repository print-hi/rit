# statistics functions

afl <- function(life_table) {# average future life time
  # we will sum up over all the survival probabilities
  average_life <- 0
  for (i in 2:nrow(life_table)) {
    average_life = average_life + life_table[i, 'Lx']/life_table[1, 'Lx']
  }
  return(average_life)
}

afld <- function(init_age, init_state, n = 10000, trans_probs) { # average future lifetime disabled
  # we do this by simulating stochastic lifetimes
  average_disabled_times <- c()
  for (. in 1:n) {
    simulated_path <- simulate_path(init_age, init_state, trans_probs)
    last_index <- ncol(simulated_path)
    # we use a trick to count the disabled time (assuming that transitions happen in middle of the year)
    modified <- (simulated_path[, 1:last_index-1] + simulated_path[, 2:last_index])/2
    disabled_time <- sum(modified[, -(last_index-1)] > 1 & modified[, -(last_index-1)] < 3)
    disabled_time <- disabled_time + sum(modified[, last_index-1] > 2 & modified[, last_index-1] < 3)
    average_disabled_times <- append(average_disabled_times, disabled_time/10000)
  }
  return('Mean' = mean(average_disabled_times), 'S.dev' = sd(average_disabled_times))
}
