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
#' numeric output for average time spent in disabled state
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

  if (n != as.integer(n)) {
    return('Error: Please input an integer for n.')
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


#' Average future lifetime in disabled state (frailty moded)
#'
#' Performs the same function as \code{\link[tshm]{afld}}, but it requires inputs
#' to simulate latent factor in the frailty model. This allows it to simulate the
#' the randomness from the frailty factor into the calculation as well.
#' NOTE: THIS FUNCTION SHOULD ONLY BE USED WITH THE FRAILTY MODEL. USE \code{\link[tshm]{afld}}
#' FOR STATIC AND TREND MODEL.
#'
#' @param init_age
#' integer between 65 and 110 denoting initial age of individual
#' @param init_state
#' 0 for healthy and 1 for disabled
#' @param female
#' 0 for non female, 1 for female
#' @param year
#' integer for current year
#' @param param_file
#' string for file path containing parameters of cox regression model
#' @param n
#' integer representing number of unique latent factors to simulate
#'
#' @return
#' numeric output for average time spent in disabled state
#'
#' @export
#'
#' @examples
afldF <- function(init_age, init_state, female, year, param_file, n = 3000) {
  # flagging errors
  if (init_age < 65 | init_age > 110) {
    return('Error: Please enter an age between 65 and 110.')
  }

  if (init_state != 0 & init_state != 1) {
    return('Error: Please input 0 (healthy) or 1 (disabled) for initial state.')
  }

  if (female != 0 & female != 1) {
    return('Error: Please input 0 or 1 to indicate female.')
  }

  if (n != as.integer(n)) {
    return('Error: Please input an integer for n.')
  }

  # we simulate average time spent in disabled state for n different latent
  # factor simulations:
  avg_disabled_times <- c()
  for(. in 1:n) {
    # simulate transition probabilities and life time paths
    TP <- get_trans_probs('F', param_file, init_age, female, year)
    simulated_path <- simulate_path(init_age, init_state, TP)
    disabled_time <- sum(simulated_path == 1)

    # add result to simulations
    if (init_state == 1) {
      avg_disabled_times <- append(avg_disabled_times, disabled_time/10000-0.5)
    } else {
      avg_disabled_times <- append(avg_disabled_times, disabled_time/10000)
    }
  }
  return(mean(avg_disabled_times))
}

