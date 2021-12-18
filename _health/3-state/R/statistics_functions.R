# statistics functions

#' average future lifetime
#'
#' Calculates average future lifetime remaining from initial age and state. Note that the
#' average future lifetime calculates the remaining time left (regardless of state) from the
#' initial state. So remaining lifetimes for individuals starting healthy or disabled will differ.
#'
#' @param life_table
#' This is a life table generated from \code{\link[tshm]{create_life_table}}.
#' Since the creation of the life table already has an initial state that the cohort
#' is in, the function does not require the same input.
#'
#' @return
#' Numeric output for the average future lifetime. This is calculated by taking the
#' curtate expected lifetime; essentially a sum of survival probabilities.
#'
#' @export
#'
#' @examples
afl <- function(life_table) {# average future life time
  # we will sum up over all the survival probabilities
  average_life <- 0
  for (i in 2:nrow(life_table)) {
    average_life = average_life + life_table[i, 'Lx']/life_table[1, 'Lx']
  }
  return(average_life)
}

#' Average future lifetime in disabled state
#'
#' Calculates the average future lifetime that will be spent in disabled state from
#' initial age and state. This average future lifetime depends on the initial state
#' the individual is in. Generally, this is applied to non-homogeneus Markov chains,
#' so we will use stochastic simulation to calculate this statistic.
#'
#' @param init_age
#' Integer between 65 and 110 specifying the initial age of the individual
#' @param init_state
#' 0 for healthy, 1 for disabled
#' @param trans_probs
#' list of transition probability matrices, preferably generated from \code{\link[tshm]{get_trans_probs}}.
#' @param n
#' integer denoting number of simulations to make
#'
#' @return
#' Mean and standard deviation of average disabled times across n simulations.
#'
#' @export
#'
#' @examples
afld <- function(init_age, init_state, trans_probs, n = 1000) { # average future lifetime disabled
  # we do this by simulating stochastic lifetimes
  average_disabled_times <- c()
  for (. in 1:n) {
    simulated_path <- simulate_path(init_age, init_state, trans_probs, cohort = 1000)
    disabled_time <- sum(simulated_path == 1)

    # we also assume that transitions occur in the middle of the year, so we need to
    # count extra 1 year per block of time spent in disabled

    for (i in 1:nrow(simulated_path)) {
      indices <- which(simulated_path[i, ] == 1)
      transitions <- indices[2:length(indices)] - indices[1:(length(indices)-1)]
      disabled_time <- disabled_time + sum(transitions != 1) + 1
    }

    # if individuals started on disabled, then we have over accounted 0.5 years of disabled time
    # for each individual in the simulation
    if (init_state == 1) {
      disabled_time <- disabled_time - 0.5*nrow(simulated_path)
    }
    average_disabled_time <- append(average_disabled_time, disabled_time)
  }
  return(c('Mean' = mean(average_disabled_times), 'S.dev' = sd(average_disabled_times)))
}

