# Sample code for 3 state health module (tshm)
library(gridExtra)

# load all modules
devtools::load_all()

# individual characteristics
init_age <- 77
init_state <- 0
female <- 1
year <- 2022

# Transition probabilities ---------------
trans_probsS <- get_trans_probs('S', US_HRS, init_age, female, year)
trans_probsT <- get_trans_probs('T', US_HRS, init_age, female, year)

# fraily model transition probability matrices are stochastic
trans_probsF <- get_trans_probs('F', US_HRS, init_age, female, year)


# Life tables and plots -------------------
lifetableS <- create_life_table(trans_probsS, init_age, init_state)
lifetableT <- create_life_table(trans_probsT, init_age, init_state)

# frailty life table should use the 'F' version of the function
# note: all frailty functions take substantially longer to run dueo the number of simulations
lifetableF <- create_life_tableF(init_age, female, year, US_HRS, init_state)

# create plots
s_plot <- prob_plots(init_state, init_age, trans_probsS) + ggtitle('Static Model Plot')
t_plot <- prob_plots(init_state, init_age, trans_probsT) + ggtitle('Trend Model Plot')
f_plot <- prob_plots(init_state, init_age, trans_probsF) + ggtitle('Frailty Model Plot')

grid.arrange(s_plot, t_plot, f_plot, ncol = 2)


# Path simulation ---------------------
simulationS <- simulate_path(init_age, init_state, trans_probsS)
simulationT <- simulate_path(init_age, init_state, trans_probsT)

# the smulation of the frailty model is conducted on the same latent factor path
simulationF <- simulate_path(init_age, init_state, trans_probsF)

# if you want random simulations, then you loop over new simulations of trans_probsF


# Statistics ---------------------

# can derive individual statistics, or can bulk generate statistics

# average future lifetime
afl(init_age, init_state, trans_probsS)
afl(init_age, init_state, trans_probsT)

# frailty version of function
aflF(init_age, init_state, female, year, US_HRS)



# healthy future lifetime
hfl(init_age, init_state, trans_probsS)
hfl(init_age, init_state, trans_probsT)

# frailty version of function
hflF(init_age, init_state, female, year, US_HRS)



# disabled future lifetime
afld(init_age, init_state, trans_probsS)
afld(init_age, init_state, trans_probsT)

# frailty version of function
afldF(init_age, init_state, female, year, US_HRS)



# time until onset of disability
time_to_disabled(init_age, trans_probsS)
time_to_disabled(init_age, trans_probsT)

# frailty version of function
time_to_disabledF(init_age, trans_probsF)


# all stats
survival_stats(init_age, init_state, trans_probsS)
survival_stats(init_age, init_state, trans_probsT)

# frailty version
survival_statsF(init_age, init_state, female, year, US_HRS)






