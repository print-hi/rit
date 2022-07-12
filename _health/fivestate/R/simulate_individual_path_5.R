#' function to get the path of 10000 individuals
#'
#' @param init_age
#' the initial age of the path
#' @param init_state
#' the initial state of all individuals
#' @param cohort
#' the number of simulations, set to be 10000
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#'
#' @return
#' 10000 rows of individual paths in the states, 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead
#' for each row it starts from the initial age as an input, and end at age 110
#' @export
#'
#' @examples
#' simulated_individual_path=simulate_individual_path_5(init_age=65, init_state=0, params, gender, i, cohort = 10000, model=3)
simulate_individual_path_5 <- function(init_age, init_state, params, gender, i, cohort = 10000, model) {
  # init_state 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead

  # create empty matrix to contain simulated population
  simulated_pop <- matrix(0, nrow = cohort, ncol = 110-init_age+2)

  # initialise all individuals
  simulated_pop[, 1] <- init_state

  trans_prob_matrix=get_full_trans_prob_matrix_5(params, init_age, gender, i, model)

  for (i in 2:ncol(simulated_pop)) {
    simulated_pop[simulated_pop[,i-1] == 0, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 0),
                                                         replace = TRUE,
                                                         prob = trans_prob_matrix[[i-1]][1, ])


    simulated_pop[simulated_pop[,i-1] == 1, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 1),
                                                         replace = TRUE,
                                                         prob = trans_prob_matrix[[i-1]][2, ])

    simulated_pop[simulated_pop[,i-1] == 2, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 2),
                                                         replace = TRUE,
                                                         prob = trans_prob_matrix[[i-1]][3, ])


    simulated_pop[simulated_pop[,i-1] == 3, i] <- sample(c(0, 1, 2, 3, -1),
                                                         sum(simulated_pop[, i-1] == 3),
                                                         replace = TRUE,
                                                         prob = trans_prob_matrix[[i-1]][4, ])

    simulated_pop[simulated_pop[,i-1] == -1, i] <- -1
  }
  return(simulated_pop)
}

