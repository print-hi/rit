#setwd("~/Documents/GitHub/retirement-toolkit/_health/fivestate/R")
#install.packages("roxygen2")
#install.packages("devtools")
#install.packages("expm")
#install.packages("readxl")
##
library("roxygen2")
library("devtools")
library("expm")
library("readxl")

devtools::install("~/Documents/GitHub/retirement-toolkit/_health/fivestate")
library("fivestate")

# read data
my_data <- read_excel("~/Documents/GitHub/retirement-toolkit/_health/fivestate/R/Params_US_5_state.xlsx",col_names=TRUE)
params_5_no_frailty <- my_data[1:3,3:ncol(my_data)] # model 3
params_5_no_frailty_with_trend <- my_data[5:8,3:ncol(my_data)] # model 4
params_5_frailty <- my_data[10:14,3:ncol(my_data)] # model 5

# select the parameter and model
# model=1 no frailty model
# model=2 no frailty model with trend
# model=3 frailty model
params=params_5_frailty
model=3

# input characteristics of the individual at time 0
init_age=65
gender=0
i=9 # wave index
latent=0 # initial value of latent factor


##### test model 3 #####
transition_rates=transition_rate_5_frailty(params, init_age, gender, i, latent, model=3)
transition_probabilities=transition_probability_5_frailty(params, init_age, gender, i, latent, model=3)
###
trans_prob_matrix_age65to110=get_full_trans_prob_matrix(params, init_age=65, gender, i, model=3)
simulated_individual_path=simulate_individual_path(init_age=65, init_state=0, params, gender, i, cohort = 10000, model=3)
simulated_lifetable=simulate_life_table(params,age,gender,i,latent,n_sim=100, model=3) #model=3 in this case because this function is for simulating the latent factor, otherwise it will produce 100 same simulations, use n_sim=1 for model 1 and 2
###
first_time_H=first_time_stats(simulated_individual_path, 0)
total_time_alive=total_time_stats(simulated_individual_path, 4)
##
stats_first_time_H=stats_produce(first_time_H)
stats_first_time_H

stats_total_time_alive=stats_produce(total_time_alive)
stats_total_time_alive

# unload the package
# detach("package:fivestate", unload=TRUE)

##### test model 1 #####
transition_rates=transition_rate_5_frailty(params_5_no_frailty, init_age, gender, i, latent, model=1)
transition_probabilities=transition_probability_5_frailty(params_5_no_frailty, init_age, gender, i, latent, model=1)
###
trans_prob_matrix_age65to110=get_full_trans_prob_matrix(params_5_no_frailty, init_age=65, gender, i, model=1)
simulated_individual_path=simulate_individual_path(init_age=65, init_state=0, params_5_no_frailty, gender, i, cohort = 10000, model=1)
simulated_lifetable=simulate_life_table(params_5_no_frailty,age,gender,i,latent,n_sim=10, model=1) #it will produce same simulations for model 1 and 2
###
first_time_H=first_time_stats(simulated_individual_path, 0)
total_time_alive=total_time_stats(simulated_individual_path, 4)
##
stats_first_time_H=stats_produce(first_time_H)
stats_first_time_H

stats_total_time_alive=stats_produce(total_time_alive)
stats_total_time_alive

##### test model 2 #####
transition_rates=transition_rate_5_frailty(params_5_no_frailty_with_trend, init_age, gender, i, latent, model=2)
transition_probabilities=transition_probability_5_frailty(params_5_no_frailty_with_trend, init_age, gender, i, latent, model=2)
###
trans_prob_matrix_age65to110=get_full_trans_prob_matrix(params_5_no_frailty_with_trend, init_age=65, gender, i, model=2)
simulated_individual_path=simulate_individual_path(init_age=65, init_state=0, params_5_no_frailty_with_trend, gender, i, cohort = 10000, model=2)
simulated_lifetable=simulate_life_table(params_5_no_frailty_with_trend,age,gender,i,latent,n_sim=10, model=2) #it will produce same simulations for model 1 and 2
###
first_time_H=first_time_stats(simulated_individual_path, 0)
total_time_alive=total_time_stats(simulated_individual_path, 4)
##
stats_first_time_H=stats_produce(first_time_H)
stats_first_time_H

stats_total_time_alive=stats_produce(total_time_alive)
stats_total_time_alive

