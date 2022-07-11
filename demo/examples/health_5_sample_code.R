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
# load("~/Documents/GitHub/retirement-toolkit/demo/data/US_HRS_5.rda")
#params_5_static <- US_HRS_5[1:3,3:ncol(US_HRS_5)] # static model
#params_5_trend <- US_HRS_5[4:7,3:ncol(US_HRS_5)] # trend model
#params_5_frailty <- US_HRS_5[8:12,3:ncol(US_HRS_5)] # frailty model

#save(US_HRS_5, file="data/US_HRS_5.rda")

#save(params_5_static, file="data/params_5_static.rda")
#save(params_5_trend, file="data/params_5_trend.rda")
#save(params_5_frailty, file="data/params_5_frailty.rda")
# select the parameter and model
# model='S' static model
# model='T' trend model
# model='F' frailty model
params=US_HRS_5
model='F'

# input characteristics of the individual at time 0
init_age=65
gender=0
i=8 # wave index
latent=0 # initial value of latent factor

##### test frailty model #####
transition_rates=health5_get_trans_rates(model='F', params, init_age, gender, i, latent)
transition_probabilities=health5_get_trans_probs(model='F',params, init_age, gender, i, latent)
###
trans_prob_matrix_age65to110=health5_get_list_trans_prob_matrix(model='F',params, init_age=65, gender, i)
###
simulated_lifetable=health5_get_life_table(model='F',params,init_age=65,gender,i,latent,initial_state=0, n_sim=100) #model='F in this case because this function is for simulating the latent factor, otherwise it will produce 100 same simulations, use n_sim=1 for model='S' or model='T'
###
simulated_individual_path=health5_simulate_individual_path(model='F',init_age=65, init_state=0, params, gender, i, cohort = 10000)
###
first_time_H=health5_first_time_stats(simulated_individual_path, 0)
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)
###
stats_first_time_H=health5_stats_produce(first_time_H)
stats_first_time_H
### future life time given in H state
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive
###
total_time_1=health5_total_time_stats(simulated_individual_path, 0)
total_time_2=health5_total_time_stats(simulated_individual_path, 1)
total_time_3=health5_total_time_stats(simulated_individual_path, 2)
total_time_4=health5_total_time_stats(simulated_individual_path, 3)
###
stats_total_time_1=health5_stats_produce(total_time_1)
stats_total_time_1
#
stats_total_time_2=health5_stats_produce(total_time_2)
stats_total_time_2
#
stats_total_time_3=health5_stats_produce(total_time_3)
stats_total_time_3
#
stats_total_time_4=health5_stats_produce(total_time_4)
stats_total_time_4
### future life time given in H state
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive

### future life time given in M state
simulated_individual_path=health5_simulate_individual_path(model='F',init_age=65, init_state=1, params, gender, i, cohort = 10000)
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive
# unload the package
# detach("package:fivestate", unload=TRUE)

##### test static model #####
transition_rates=health5_get_trans_rates(model='S', params, init_age, gender, i, latent)
transition_probabilities=health5_get_trans_probs(model='S', params, init_age, gender, i, latent)
###
trans_prob_matrix_age65to110=health5_get_list_trans_prob_matrix(model='S', params, init_age=65, gender, i)
simulated_lifetable=health5_get_life_table(model='S',params,init_age=65,gender,i,latent,initial_state=0,n_sim=1) #it will produce same simulations for model='S' or model='T', so n_sim is set to be 1
###
simulated_individual_path=health5_simulate_individual_path(model='S',init_age=65, init_state=0, params, gender, i, cohort = 10000)
###
first_time_H=health5_first_time_stats(simulated_individual_path, 0)
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)
##
stats_first_time_H=health5_stats_produce(first_time_H)
stats_first_time_H

stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive
###
total_time_1=health5_total_time_stats(simulated_individual_path, 0)
total_time_2=health5_total_time_stats(simulated_individual_path, 1)
total_time_3=health5_total_time_stats(simulated_individual_path, 2)
total_time_4=health5_total_time_stats(simulated_individual_path, 3)
###
stats_total_time_1=health5_stats_produce(total_time_1)
stats_total_time_1
#
stats_total_time_2=health5_stats_produce(total_time_2)
stats_total_time_2
#
stats_total_time_3=health5_stats_produce(total_time_3)
stats_total_time_3
#
stats_total_time_4=health5_stats_produce(total_time_4)
stats_total_time_4

##### test trend model #####
transition_rates=health5_get_trans_rates(model='T',params, init_age, gender, i, latent)
transition_probabilities=health5_get_trans_probs(model='T',params, init_age, gender, i, latent)
###
trans_prob_matrix_age65to110=health5_get_list_trans_prob_matrix(model='T',params, init_age=65, gender, i)
simulated_lifetable=health5_get_life_table(model='T',params,init_age=65,gender,i,latent,initial_state=0,n_sim=1) #it will produce same simulations for model='S' or model='T'
###
simulated_individual_path=health5_simulate_individual_path(model='T',init_age=65, init_state=0, params, gender, i, cohort = 10000)
###
first_time_H=health5_first_time_stats(simulated_individual_path, 0)
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)
##
stats_first_time_H=health5_stats_produce(first_time_H)
stats_first_time_H

stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive
###
total_time_1=health5_total_time_stats(simulated_individual_path, 0)
total_time_2=health5_total_time_stats(simulated_individual_path, 1)
total_time_3=health5_total_time_stats(simulated_individual_path, 2)
total_time_4=health5_total_time_stats(simulated_individual_path, 3)
###
stats_total_time_1=health5_stats_produce(total_time_1)
stats_total_time_1
#
stats_total_time_2=health5_stats_produce(total_time_2)
stats_total_time_2
#
stats_total_time_3=health5_stats_produce(total_time_3)
stats_total_time_3
#
stats_total_time_4=health5_stats_produce(total_time_4)
stats_total_time_4
