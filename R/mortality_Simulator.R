#' Simulate Cohort Life Path
#'
#' Simulates the path each life takes in an initial cohort using 1-year death probabilities
#'
#' @param init_age
#' the initial age of the path
#' @param sex
#' character string denoting the gender of individuals, "F" for female and "M" for male
#' @param death_probs
#' a vector of 1-year death probabilities. If not supplied, an M7 age-period-cohort
#' model will be fitted on \code{mortality_AUS_data} to produce forecasted
#' death probabilities for a cohort starting at init_age in 2022
#' @param closure_age
#' maximum life span
#' @param cohort
#' integer (default 10000) denoting number of people in the simulation
#'
#' @return
#' a matrix where each row represents a new individual, and the columns represent
#' whether the individual is dead (-1) or alive (0)
#'
#' @export
#'
#' @examples
#'
simulate_path_mortality <- function(init_age, sex = "F", death_probs = NULL, closure_age = 130, cohort = 10000) {

# Flagging errors ---------------------------------------------------------

    # init_age
    if (init_age < 55 | init_age > closure_age) {
        stop("initial age must be between 55 and the maximum age")
    }

    if (init_age != floor(init_age)) {
        stop("initial age must be an integer")
    }

    # sex
    if (sex != "F" & sex != "M") {
        stop("sex must be 'F' or 'M'")
    }

    # death_probs
    if (!is.null(death_probs)) {
        if (!is.vector(death_probs) | !is.numeric(death_probs)) {
            stop("death probabilities must be a numeric vector")
        }

        if (any(death_probs < 0, na.rm = T) | any(death_probs > 1, na.rm = T) ) {
            stop("1-yr death probabilities must be between 0 and 1")
        }

        if (length(death_probs) != closure_age - init_age + 1) {
            stop("number of death probabilities does not correspond to the given initial and max age")
        }
    }
    # closure_age
    if (closure_age < 90 | closure_age != floor(closure_age)) {
        stop("maximum age must be an integer greater than 89")
    }

    # cohort
    if (cohort < 0 | cohort != floor(cohort)) {
        stop('cohort must be a positive integer')
    }

# Implementation ----------------------------------------------------------

    # Generating default death probabilities for males and females if required
    if (is.null(death_probs)) {
        young_ages <- 55:89

        if (sex == "F") {
            AUS_StMoMo <- StMoMoData(mortality_AUS_data, series = "female")
            rates_hist <- mortality_AUS_data$rate$female[as.character(young_ages), ]
        } else {
            AUS_StMoMo <- StMoMoData(mortality_AUS_data, series = "male")
            rates_hist <- mortality_AUS_data$rate$male[as.character(young_ages), ]
        }

        # Using M7 model to Forecast Rates
        M7 <- m7()
        AUS_Ini_Data <- central2initial(AUS_StMoMo)
        ages_fit <- young_ages
        wxy <- genWeightMat(ages = ages_fit, years = AUS_Ini_Data$years, clip = 3)
        M7_fit <- fit(M7, data = AUS_Ini_Data, ages.fit = ages_fit, wxt = wxy)
        M7_for <- forecast(M7_fit, h = 100)

        # Mortality Rate Completion with Kannisto Method
        old_ages <- 90:closure_age
        ages <- c(young_ages, old_ages)
        kannisto_hist <- complete_old_age(rates = rates_hist, ages = young_ages,
                                          old_ages = old_ages, fitted_ages = 80:89,
                                          closure_age = closure_age,
                                          method = "kannisto", type = "central")
        kannisto_for <- complete_old_age(rates = M7_for$rates, ages = young_ages,
                                         old_ages = old_ages, fitted_ages = 80:89,
                                         closure_age = closure_age,
                                         method = "kannisto", type = "central")

        # Combine Historical and Forecasted Rates
        kannisto_55_period <- cbind(kannisto_hist, kannisto_for)
        qx_period <- rate2rate(kannisto_55_period, from = "central", to = "prob")
        # Take year 2022
        death_probs <- period2cohort(qx_period, ages = ages, init_age = init_age)[, "2022"]

    }

    # create empty matrix of simulated population
    sim_pop <- matrix(0, nrow = cohort, ncol = closure_age - init_age+2)

    # qx should be vector of length (closure_age - init_age + 1)
    for (i in 2:ncol(sim_pop)) {
        # Simulate TRUE/FALSE vector indicating if individual has died
        # during this transition
        sim_death <- (runif(cohort) <= death_probs[i - 1])
        # Encoding to 0 for alive and -1 for dead
        sim_pop[, i] <- -as.numeric(sim_death)
        # transitions cannot occur for dead individuals
        sim_pop[sim_pop[, i-1] == -1, i] <- -1
    }
    return(sim_pop)

}

