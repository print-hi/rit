# function to produce transition probability matrices

#' Calculate transition probability matrices
#'
#' Creates a list of transition probability matrices starting from initial age to
#' max age of 110 for 3 state model
#'
#' @param model_type
#' string that selects model type; S for Static, T for Trend and F for Frailty
#' @param param_file
#' string for file path of parameter file OR a tibble/dataframe of parameters
#' @param init_age
#' integer denoting age of policy holder
#' @param female
#' takes values 1 or 0, where 1 indicates policyholder is female
#' @param year
#' integer denoting current year
#'
#' @return
#' list of transition probability matrices
#'
#' @import expm
#' @examples example
#'
health3_get_trans_probs <- function(model_type, param_file, init_age, female, year) {
    b1 <- gamma_age1 <- gamma_gender1 <- gamma_time1 <- a1 <- NULL
    b2 <- gamma_age2 <- gamma_gender2 <- gamma_time2 <- a2 <- NULL
    b3 <- gamma_age3 <- gamma_gender3 <- gamma_time3 <- a3 <- NULL
    b4 <- gamma_age4 <- gamma_gender4 <- gamma_time4 <- a4 <- NULL
    # flagging errors
    if (init_age < 65 | init_age >= 110) {
      stop('invalid age')
    }

    if (as.integer(init_age) != init_age) {
      stop('initial age must be an integer')
    }

    if (female != 0 & female != 1) {
      stop('invalid gender')
    }

    if (year != floor(year)) {
      stop('year must be an integer')
    }

    if (!is.character(param_file)) {
      if (nrow(param_file) != 15 | ncol(param_file) != 6) {
        stop('incorrect size for parameter file')
      }
    }

    if (year < 1998 | year > 2999) {
      stop('year must be within reasonable range: 1998 to 2999 inclusive')
    }

    # extract parameter values from parameter file
    if (is.character(param_file)) {
      param_list <- readxl::read_excel(param_file)
    } else {
      param_list <- param_file
    }

    if (model_type == 'S') {
      params <- param_list[1:5, ]
      v <- 0 # no stochastic component
    } else if (model_type == 'T') {
      params <- param_list[6:10, ]
      v <- 0 # no stochastic component
    } else if (model_type == 'F') {
      params <- param_list[11:15, ]

      # we also need to simulate v
      # we will simulate 300 time points, which will be sufficient until year 2297
      v <- rep(0, 301)
      for (i in 2:301) {
        v[i] <- v[i-1] + stats::rnorm(1)
      }
    } else {
      stop('invalid model type')
    }

    # Get parameters for each of the 4 transition types
    for (i in 1:4) {
      assign(paste('b', i, sep = ''), as.numeric(params[1, i+2]))
      assign(paste('gamma_age', i, sep = ''), as.numeric(params[2, i+2]))
      assign(paste('gamma_gender', i, sep = ''), as.numeric(params[3, i+2]))
      assign(paste('gamma_time', i, sep = ''), as.numeric(params[4, i+2]))
      assign(paste('a', i, sep = ''), as.numeric(params[5, i+2]))
    } # We now have the coefficients for each of the 4 transition types

    # create cox regression function
    cox_model <- function(target_age, init_age, female, year, model_type, b, gamma_age,
                          gamma_gender, gamma_time, a, v) {

      # we need target t, a coefficient in the cox model
      init_t <- year-1998+1
      t <- target_age - init_age + init_t

      # give hazard rate output
      if (model_type != 'F') {
        return(exp(b + gamma_age*(floor(target_age)-65)/10 + gamma_gender*female + gamma_time*floor(t)/10))
      } else {
        v_t <- v[floor(t)+1]
        # now we return hazard rate output
        return(exp(b + gamma_age*(floor(target_age)-65)/10 + gamma_gender*female + gamma_time*floor(t)/10 +
                     a*v_t))
      }
    }

    # transition 1 rates
    trans1 <- c()
    for (i in init_age:109) {
      # integrate across each year to get annual piecewise transition rates
      integral <- stats::integrate(cox_model, i, i+1, init_age = init_age, female = female,
                            year = year, model_type = model_type, b = b1, gamma_age = gamma_age1,
                            gamma_gender = gamma_gender1, gamma_time = gamma_time1, a = a1, v = v)
      trans1 <- append(trans1, integral$value)
    }


    # transition 2 rates
    trans2 <- c()
    for (i in init_age:109) {
      integral <- stats::integrate(cox_model, i, i+1, init_age = init_age, female = female,
                            year = year, model_type = model_type, b = b2, gamma_age = gamma_age2,
                            gamma_gender = gamma_gender2, gamma_time = gamma_time2, a = a2, v = v)
      trans2 <- append(trans2, integral$value)
    }


    # transition 3 rates
    trans3 <- c()
    for (i in init_age:109) {
      integral <- stats::integrate(cox_model, i, i+1, init_age = init_age, female = female,
                            year = year, model_type = model_type, b = b3, gamma_age = gamma_age3,
                            gamma_gender = gamma_gender3, gamma_time = gamma_time3, a = a3, v = v)
      trans3 <- append(trans3, integral$value)
    }


    # transition 4 rates
    trans4 <- c()
    for (i in init_age:109) {
      integral <- stats::integrate(cox_model, i, i+1, init_age = init_age, female = female,
                            year = year, model_type = model_type, b = b4, gamma_age = gamma_age4,
                            gamma_gender = gamma_gender4, gamma_time = gamma_time4, a = a4, v = v)
      trans4 <- append(trans4, integral$value)
    }


    # we now use the transition rates we have to create transition rate matrices Q
    Qs <- list()
    for (i in 1:length(trans1)) {
      Q <- matrix(c(-trans1[i]-trans3[i], trans1[i], trans3[i],
                    trans2[i], -trans2[i]-trans4[i], trans4[i],
                    0, 0, 0), nrow = 3, byrow = TRUE)
      Qs[[i]] <- Q
    }

    # take matrix exponentials to find Ps; the transition probability matrices
    Ps <- lapply(Qs, expm::expm)

    # append last transition where everyone dies (will make lifetable section easier)
    Ps[[length(Ps)+1]] <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 1), nrow = 3, byrow = TRUE)
    return(Ps)
}

























