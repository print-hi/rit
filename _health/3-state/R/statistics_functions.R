# statistics functions

#' Average future lifetime
#'
#' Calculates the average future life time given initial state and age of an
#' individual. This is calculated using the curtate expected life time, which is
#' essentially a sum survival probabilities.
#' NOTE: USE \code{\link[tshm]{aflF}} for frailty model.
#'
#' @param init_age
#' integer between 65 and 110 denoting initial age of individual
#'
#' @param init_state
#' 0 for healthy, 1 for disabled
#'
#' @param trans_probs
#' a list of transition probability matrices, preferably generated from
#' \code{\link[tshm]{get_trans_probs}}.
#'
#' @return
#' numeric output for average future lifetime
#'
#' @export
#'
#' @examples
afl <- function(init_age, init_state, trans_probs) {
  # screening for errors
  if (init_state != 0 & init_state != 1) {
    return('Please enter a valid initial state: 0 for healthy, 1 for disabled.')
  }

  if (init_age<65 | init_age>110) {
    return('Error: init_age outside bounds of allowable age values')
  }

  # we calculate survival probs for each age and sum
  probs <- c()
  for (i in 1:(110-init_age)) {
    prob <- tshm::surv_prob(init_state, init_age, init_age+i, trans_probs)
    probs <- append(probs, prob)
  }
  return(sum(probs) + 0.5)
}



#' Average future lifetime (Frailty Model)
#'
#' Performs the same function as the \code{\link[tshm]{afl}}, but simulates a number of
#' unique latent factor paths to make probabilities stochastic.
#' NOTE: USE \code{\link[tshm]{afl}} for static and trend models.
#'
#' @param init_age
#' integer between 65 and 110 indicating initial age of individual
#'
#' @param init_state
#' 0 for healthy, 1 for disabled
#'
#' @param female
#' 0 for male, 1 for female
#'
#' @param year
#' integer indicating current year
#'
#' @param param_file
#' string name of file containing all the parameters of the model
#'
#' @param n
#' integer denoting number of unique latent factor simulations
#'
#'
#' @return
#' numeric output for expected future lifetime
#'
#' @export
#'
#' @examples
aflF <- function(init_age, init_state, female, year, param_file, n = 5000) {
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

  afls <- c()
  for (. in 1:n) {
    # simulate new frailty path for each iteration
    TP <- tshm::get_trans_probs('F', param_file, init_age, female, year)
    future_lifetime <- tshm::afl(init_age, init_state, TP)
    afls <- append(afls, future_lifetime + 0.5)
  }
  return(mean(afls))
}


hfl <- function(init_age, init_state, trans_probs) {
  # screening for errors
  if (init_state != 0 & init_state != 1) {
    return('Please enter a valid initial state: 0 for healthy, 1 for disabled.')
  }

  if (init_age<65 | init_age>110) {
    return('Error: init_age outside bounds of allowable age values')
  }

  # we sum up probabilities of being disabled in each state
  # this is similar to curtate life expectation
  probs <- c()
  for (i in 1:(110-init_age)) {
    prob <- tshm::surv_prob(init_state, init_age, init_age+i, trans_probs, end_state = 0)
    probs <- append(probs, prob)
  }
  if (init_state == 0) {
    return(sum(probs) + 0.5) # extra half year of disabled at the start
  } else {
    return(sum(probs))
  }
}


#' Average future lifetime in disabled state
#'
#' Calculates the average future lifetime spent in disabled state using a similar
#' idea to the curtate expected life. Function sums up transition probability into
#' disabled state for each year.
#' NOTE: THIS FUNCTION SHOULD ONLY BE USED WITH STATIC AND TREND MODELS. USE
#' \code{\link[tshm]{afldF}} FOR FRAILTY MODEL.
#'
#' @param init_age
#' integer between 65 and 110 denoting age of individual
#'
#' @param init_state
#' 0 for healthy, 1 for disabled
#'
#' @param trans_probs
#' a list of transition probability matrices, preferably generated from
#' \code{\link[tshm]{get_trans_probs}}.
#'
#' @return
#' numeric output for average future lifetime in disabled state
#'
#' @export
#'
#' @examples
afld <- function(init_age, init_state, trans_probs) {
  # screening for errors
  if (init_state != 0 & init_state != 1) {
    return('Please enter a valid initial state: 0 for healthy, 1 for disabled.')
  }

  if (init_age<65 | init_age>110) {
    return('Error: init_age outside bounds of allowable age values')
  }

  # we sum up probabilities of being disabled in each state
  # this is similar to curtate life expectation
  probs <- c()
  for (i in 1:(110-init_age)) {
    prob <- tshm::surv_prob(init_state, init_age, init_age+i, trans_probs, end_state = 1)
    probs <- append(probs, prob)
  }
  if (init_state == 0) {
    return(sum(probs))
  } else {
    return(sum(probs) + 0.5) # extra half year of disabled at the start
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
afldF <- function(init_age, init_state, female, year, param_file, n = 5000) {
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
    disabled_time <- tshm::afld(init_age, init_state, TP)
    avg_disabled_times <- append(avg_disabled_times, disabled_time)
  }
  return(mean(avg_disabled_times))
}




#' Time until onset of disability (conditional on being disabled)
#'
#' Uses simulation to produce an average time until a healthy individual becomes disabled
#' during their life time. Note that this is conditional on the individual becoming disabled,
#' so it doesn't factor in life simulations where the indiviudal is always healthy.
#' NOTE: USE \code{\link[tshm]{time_to_disabledF}} for frailty model.
#'
#'
#' @param init_age
#' integer between 65 and 110 denoting initial age of individual
#'
#' @param trans_probs
#' a list of transition probability matrices, preferably generated from
#' \code{\link[tshm]{get_trans_probs}}.
#'
#' @return
#' numeric denoting the average time until indiviudal becomes disabled
#'
#' @export
#'
#' @examples
time_to_disabled <- function(init_age, trans_probs) {
  # screening for errors
  if (init_age<65 | init_age>110) {
    return('Error: init_age outside bounds of allowable age values')
  }

  # create a simulation
  simulated_path <- tshm::simulate_path(init_age, 0, trans_probs, cohort = 5000)

  first_time <- c()
  for (i in 1:nrow(simulated_path)) {
    row_val <-simulated_path[i, ]
    if (1 %in% row_val) {
      first_time <- append(first_time, match(1, row_val)-0.5)
    }
  }
  return(mean(first_time))
}



#' Time until onset of disability (Frailty model)
#'
#' Function serves the same purpose as \code{\link[tshm]{time_to_disabled}}, but it is for
#' the frailty model.
#'
#' @param init_age
#' integer between 65 and 110
#'
#' @param female
#' 0 for male, 1 for female
#'
#' @param year
#' integer denoting current year
#'
#' @param param_file
#' string for file path containing parameters of cox regression model
#'
#' @param n
#' integer denoting number of simulations to make
#'
#' @return
#' numeric denoting the average time until indiviudal becomes disabled
#'
#' @export
#'
#' @examples
time_to_disabledF <- function(init_age, female, year, param_file, n = 2500) {
  # flagging errors
  if (init_age < 65 | init_age > 110) {
    return('Error: Please enter an age between 65 and 110.')
  }

  if (female != 0 & female != 1) {
    return('Error: Please input 0 or 1 to indicate female.')
  }

  if (n != as.integer(n)) {
    return('Error: Please input an integer for n.')
  }

  # create n unique latent factor paths
  ttds <- c() # vector to contain each time to disabled
  for (. in 1:n) {
    TP <- tshm::get_trans_probs('F', param_file, init_age, female, year)
    ttd <- tshm::time_to_disabled(init_age, TP)
    ttds <- append(ttds, ttd)
  }
  return(mean(ttds))
}


