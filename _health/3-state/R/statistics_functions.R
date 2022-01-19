# statistics functions

#' Average future lifetime
#'
#' Calculates the average future life time and its standard deviation
#' given initial state and age of an individual by simulating life time paths.
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
#' numeric output for average and standard deviation of future lifetime
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

  # simulate path
  SP <- simulate_path(init_age, init_state, trans_probs)
  # count time at death
  future_lifetimes <- rep(0, nrow(SP))
  for (i in 1:nrow(SP)) {
    row_val = SP[i, ]
    future_lifetimes[i] <- which(row_val == -1)[1]-1-0.5 # assume transition happens mid year
  }
  return(c('mean' = mean(future_lifetimes), 's.dev' = sd(future_lifetimes)))
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
#' numeric output for expected future lifetime and standard deviation of future lifetime
#'
#' @export
#'
#' @examples
aflF <- function(init_age, init_state, female, year, param_file, n = 1000) {
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

  future_lifetimes <- rep(0, n*10000)
  for (x in 1:n) {
    # simulate new frailty path for each iteration
    TP <- tshm::get_trans_probs('F', param_file, init_age, female, year)
    SP <- tshm::simulate_path(init_age, init_state, TP)
    for (i in 1:nrow(SP)) {
      row_val <- SP[i, ]
      future_lifetimes[(x-1)*10000+i] <- which(row_val == -1)[1]-1-0.5
    }
  }
  return(c('mean' = mean(future_lifetimes), 's.dev' = sd(future_lifetimes)))
}


#' Healthy Future lifetime
#'
#' Calculates the expected future lifetime spent and its standard deviation
#' in the healthy state.
#' NOTE: USE \code{\link[tshm]{hflF}} FOR FRAILTY MODEL.
#'
#' @param init_age
#' integer between 65 and 110 denoting initial age of individual
#'
#' @param init_state
#' 0 for healthy, 1 for disabled
#'
#' @param trans_probs
#' list of transition probability matrices; ideally generated from
#' \code{\link[tshm]{get_trans_probs}}.
#'
#' @return
#' Numeric output of expected time and standard deviation of time spent in healthy state.
#'
#' @export
#'
#' @examples
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
  SP <- tshm::simulate_path(init_age, init_state, trans_probs)
  healthy_lifetimes <- rep(0, nrow(SP))
  for (i in 1:nrow(SP)) {
    row_val <- SP[i,]
    if (init_state == 0) {
      healthy_lifetimes[i] <- sum(row_val == 0) - 0.5 # transition happens in middle of period
    } else {
      healthy_lifetimes[i] <- sum(row_val == 0)
    }
  }
  return(c('mean' = mean(healthy_lifetimes), 's_dev' = sd(healthy_lifetimes)))
}

#' Heatlhy Future Lifetime (Frailty Model)
#'
#' Performs the same function as \code{\link[tshm]{hfl}}, but simulates unique latent factor
#' paths in the process.
#' NOTE: USE \code{\link[tshm]{hfl}} for static and trend models.
#'
#' @param init_age
#' integer between 65 and 110 denoting initial age of individual
#'
#' @param init_state
#' 0 for healthy, 1 for disabled
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
#' integer denoting number of simulations
#'
#' @return
#' Numeric output denoting the average time spent in healthy state and its
#' standard deviation.
#'
#' @export
#'
#' @examples
hflF <- function(init_age, init_state, female, year, param_file, n = 1000) {
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

  healthy_lifetimes <- rep(0, n*10000)
  for (x in 1:n) {
    TP <- tshm::get_trans_probs('F', param_file, init_age, female, year)
    SP <- tshm::simulate_path(init_age, init_state, TP)
    for (i in 1:nrow(SP)) {
      row_val <- SP[i,]
      if (init_state == 0) {
        healthy_lifetimes[(x-1)*10000+i] <- sum(row_val == 0) - 0.5
      } else {
        healthy_lifetimes[(x-1)*10000+i] <- sum(row_val == 0)
      }
    }
  }
  return(c('mean' = mean(healthy_lifetimes), 's_dev' = sd(healthy_lifetimes)))
}



#' Average future lifetime in disabled state
#'
#' Calculates the average future lifetime spent in disabled state and the standard
#' deviation by simulating life time paths given a list of transition probability
#' matrices.
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
#' numeric output for average future lifetime in disabled state and its standard
#' deviation.
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

  # simulate path and count disabled time
  SP <- tshm::simulate_path(init_age, init_state, trans_probs)
  disabled_lifetime <- rep(0, nrow(SP))
  for (i in 1:nrow(SP)) {
    row_val <- SP[i, ]
    if (init_state == 1) {
      disabled_lifetime[i] <- sum(row_val == 1) -0.5 # transition happens mid year
    } else {
      disabled_lifetime[i] <- sum(row_val == 1)
    }
  }
  return(c('mean' = mean(disabled_lifetime), 's_dev' = sd(disabled_lifetime)))
}


#' Average future lifetime in disabled state (frailty model)
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


