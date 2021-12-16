# Functions to create different plots
surv_prob <- function(init_state, init_age, target_age, trans_probs, end_state = NULL) {
  # screening for errors
  if (init_state != 1 & init_state != 2) {
    return('Please enter a valid initial state: 1 for healthy, 2 for disabled.')
  }

  if (!is.null(end_state)) {
    if (end_state != 1 & end_state !=2) {
      return('Please enter a valid end state: default NULL for alive, 1 for healthy, 2 for disabled.')
    }
  }

  if (init_age >= target_age) {
    return('Error: Initial age is greater than or equal to target age')
  }

  if (init_age<65 | init_age>110) {
    return('Error: init_age outside bounds of allowable age values')
  }

  if (target_age < 65 | target_age > 110) {
    return('Error: end_age outside bounds of allowable age values')
  }

  # we multiply probability matrices in the trans_probs to get transition rates
  P <- Reduce('%*%', trans_probs[1:(target_age-init_age)])

  # return relevant item from the probability matrix
  if (is.null(end_state)){
    return(P[init_state,  1] + P[init_state, 2])
  } else {
    return(P[init_state, end_state])
  }
}






