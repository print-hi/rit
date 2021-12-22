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
#' NOTE: THIS FUNCTION SHOULD ONLY BE USED WITH STATIC AND TREND MODEL, USE ....
#' FOR FRAILTY MODEL.
#'
#' @param init_age
#' Integer between 65 and 110 specifying the initial age of the individual
#' @param init_state
#' 0 for healthy, 1 for disabled
#' @param trans_probs
#' list of transition probability matrices, preferably generated from \code{\link[tshm]{get_trans_probs}}.
#' @param n
#' integer denoting number of individuals in simulation
#'
#' @return
#' Mean and standard deviation of average disabled times across n simulations.
#'
#' @export
#'
#' @examples
afld <- function(init_age, init_state, trans_probs, n = 500000) { # average future lifetime disabled
  # we do this by simulating stochastic lifetimes
  # flagging errors
  if (init_age < 65 | init_age > 110) {
    return('Error: Please enter an age between 65 and 110.')
  }

  if (init_state != 0 & init_state != 1) {
    return('Error: please input 0 (healthy) or 1 (disabled) for initial state.')
  }

  simulated_path <- simulate_path(init_age, init_state, trans_probs, cohort = n)
  # count disabled times
  disabled_time <- sum(simulated_path == 1)

  if (init_state == 1) {
    return(disabled_time/n-0.5) # we count extra 0.5 years before the initial age that we need to take off
  } else {
    return(disabled_time/n)
  }
}

