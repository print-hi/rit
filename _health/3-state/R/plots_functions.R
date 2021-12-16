# Functions to create different plots
# used tidyverse

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


prob_plots <- function(init_state, init_age, trans_probs) {
  # create 3 different probabilities:
  # general alive probability, healthy, and disabled
  alive_probs <- c(1)

  if (init_state == 1) {
    healthy_probs <- c(1)
    disabled_probs <- c(0)
  } else {
    healthy_probs <- c(0)
    disabled_probs <- c(1)
  }
  for (target in (init_age+1):110) {
    alive_probs <- append(alive_probs, surv_prob(init_state, init_age, target, trans_probs))
    healthy_probs <- append(healthy_probs, surv_prob(init_state, init_age, target, trans_probs, end_state = 1))
    disabled_probs <- append(disabled_probs, surv_prob(init_state, init_age, target, trans_probs, end_state = 2))
  }

  # create age axis
  ages <- init_age:110

  # create dataframe for ggplot
  survival_df <- data.frame('age' = ages,
                              'alive' = alive_probs,
                              'healthy' = healthy_probs,
                              'disabled' = disabled_probs)

  updated_df <- survival_df %>%
    select(age, alive, healthy, disabled) %>%
    tidyr::gather(key = 'Type', value = 'value', -age)

  surv_plot <- ggplot(updated_df, aes(x = age, y = value)) +
    geom_line(aes(color = Type)) +
    scale_color_manual(labels = c('Alive', 'Disabled', 'Healthy'),
                       values = c('darkolivegreen2', 'lightcoral', 'skyblue1')) +
    title(main = 'Probability of Surviving to each Different State')

  return(surv_plot)
}



