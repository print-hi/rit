#setwd("~/Documents/GitHub/retirement-toolkit/_health/fivestate/R")
#install.packages("roxygen2")
#install.packages("devtools")
#install.packages("expm")
#install.packages("readxl")
##
#library("roxygen2")
#library("devtools")
#library("expm")
#library("readxl")

library(devtools)
load_all(export_all=FALSE)

#devtools::install("~/Documents/GitHub/retirement-toolkit/_health/fivestate")
#library("fivestate")

# read data
load("~/Documents/GitHub/retirement-toolkit/demo/data/US_HRS_5.rda")
params_5_static <- US_HRS_5[1:3,3:ncol(US_HRS_5)] # model 3
params_5_trend <- US_HRS_5[4:7,3:ncol(US_HRS_5)] # model 4
params_5_frailty <- US_HRS_5[8:12,3:ncol(US_HRS_5)] # model 5

#save(US_HRS_5, file="data/US_HRS_5.rda")

# select the parameter and model
# model=1 no frailty model
# model=2 no frailty model with trend
# model=3 frailty model
params=params_5_frailty
model=3

# input characteristics of the individual at time 0
init_age=65
gender=0
i=8 # wave index
latent=0 # initial value of latent factor

##### test model 3 #####
transition_rates=transition_rate_5(params, init_age, gender, i, latent, model=3)
transition_probabilities=transition_probability_5(params, init_age, gender, i, latent, model=3)
###
trans_prob_matrix_age65to110=get_full_trans_prob_matrix_5(params, init_age=65, gender, i, model=3)
simulated_lifetable=simulate_life_table_5(params,init_age=65,gender,i,latent,initial_state=0, n_sim=100, model=3) #model=3 in this case because this function is for simulating the latent factor, otherwise it will produce 100 same simulations, use n_sim=1 for model 1 and 2
###
simulated_individual_path=simulate_individual_path_5(init_age=65, init_state=0, params, gender, i, cohort = 10000, model=3)
###
first_time_H=first_time_stats_5(simulated_individual_path, 0)
total_time_alive=total_time_stats_5(simulated_individual_path, 4)
###
stats_first_time_H=stats_produce_5(first_time_H)
stats_first_time_H
### future life time given in H state
stats_total_time_alive=stats_produce_5(total_time_alive)
stats_total_time_alive
###
total_time_1=total_time_stats_5(simulated_individual_path, 0)
total_time_2=total_time_stats_5(simulated_individual_path, 1)
total_time_3=total_time_stats_5(simulated_individual_path, 2)
total_time_4=total_time_stats_5(simulated_individual_path, 3)
###
stats_total_time_1=stats_produce_5(total_time_1)
stats_total_time_1
#
stats_total_time_2=stats_produce_5(total_time_2)
stats_total_time_2
#
stats_total_time_3=stats_produce_5(total_time_3)
stats_total_time_3
#
stats_total_time_4=stats_produce_5(total_time_4)
stats_total_time_4
### future life time given in H state
stats_total_time_alive=stats_produce_5(total_time_alive)
stats_total_time_alive

### future life time given in M state
simulated_individual_path=simulate_individual_path_5(init_age=65, init_state=1, params, gender, i, cohort = 10000, model=3)
total_time_alive=total_time_stats_5(simulated_individual_path, 4)
stats_total_time_alive=stats_produce_5(total_time_alive)
stats_total_time_alive
# unload the package
# detach("package:fivestate", unload=TRUE)

##### test model 1 #####
transition_rates=transition_rate_5(params_5_static, init_age, gender, i, latent, model=1)
transition_probabilities=transition_probability_5(params_5_static, init_age, gender, i, latent, model=1)
###
trans_prob_matrix_age65to110=get_full_trans_prob_matrix_5(params_5_static, init_age=65, gender, i, model=1)
simulated_lifetable=simulate_life_table_5(params_5_static,init_age=65,gender,i,latent,initial_state=0,n_sim=1, model=1) #it will produce same simulations for model 1 and 2
###
simulated_individual_path=simulate_individual_path_5(init_age=65, init_state=0, params_5_static, gender, i, cohort = 10000, model=1)
###
first_time_H=first_time_stats_5(simulated_individual_path, 0)
total_time_alive=total_time_stats_5(simulated_individual_path, 4)
##
stats_first_time_H=stats_produce_5(first_time_H)
stats_first_time_H

stats_total_time_alive=stats_produce_5(total_time_alive)
stats_total_time_alive
###
total_time_1=total_time_stats_5(simulated_individual_path, 0)
total_time_2=total_time_stats_5(simulated_individual_path, 1)
total_time_3=total_time_stats_5(simulated_individual_path, 2)
total_time_4=total_time_stats_5(simulated_individual_path, 3)
###
stats_total_time_1=stats_produce_5(total_time_1)
stats_total_time_1
#
stats_total_time_2=stats_produce_5(total_time_2)
stats_total_time_2
#
stats_total_time_3=stats_produce_5(total_time_3)
stats_total_time_3
#
stats_total_time_4=stats_produce_5(total_time_4)
stats_total_time_4

##### test model 2 #####
transition_rates=transition_rate_5(params_5_trend, init_age, gender, i, latent, model=2)
transition_probabilities=transition_probability_5(params_5_trend, init_age, gender, i, latent, model=2)
###
trans_prob_matrix_age65to110=get_full_trans_prob_matrix_5(params_5_trend, init_age=65, gender, i, model=2)
simulated_lifetable=simulate_life_table_5(params_5_trend,init_age=65,gender,i,latent,initial_state=0,n_sim=1, model=2) #it will produce same simulations for model 1 and 2
###
simulated_individual_path=simulate_individual_path_5(init_age=65, init_state=0, params_5_trend, gender, i, cohort = 10000, model=2)
###
first_time_H=first_time_stats_5(simulated_individual_path, 0)
total_time_alive=total_time_stats_5(simulated_individual_path, 4)
##
stats_first_time_H=stats_produce_5(first_time_H)
stats_first_time_H

stats_total_time_alive=stats_produce_5(total_time_alive)
stats_total_time_alive
###
total_time_1=total_time_stats_5(simulated_individual_path, 0)
total_time_2=total_time_stats_5(simulated_individual_path, 1)
total_time_3=total_time_stats_5(simulated_individual_path, 2)
total_time_4=total_time_stats_5(simulated_individual_path, 3)
###
stats_total_time_1=stats_produce_5(total_time_1)
stats_total_time_1
#
stats_total_time_2=stats_produce_5(total_time_2)
stats_total_time_2
#
stats_total_time_3=stats_produce_5(total_time_3)
stats_total_time_3
#
stats_total_time_4=stats_produce_5(total_time_4)
stats_total_time_4
