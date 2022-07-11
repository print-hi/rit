library(devtools)
load_all(export_all=FALSE)

params=US_HRS_5
model='F'

# input characteristics of the individual at time 0
init_age=65
gender=0
wave_index=8 # wave index
latent=0 # initial value of latent factor

##### test frailty model #####
trans_rates=health5_get_trans_rates(model='F', params, init_age, gender, wave_index, latent)
trans_probs_5_age65=health5_get_trans_probs_at_age(model='F',params, init_age, gender, wave_index, latent)
###
trans_probs_5=health5_get_trans_probs(model='F',params, init_age=65, gender, wave_index)
###
created_lifetable=health5_create_life_table(trans_probs_5,init_age=65,init_state=0)
simulated_lifetable=health5_simulate_life_tables(model='F', params, gender, wave_index,latent=0,init_age=65,init_state=0,n_sim=100,return_expected=0)
simulated_lifetable_mean=health5_simulate_life_tables(model='F', params, gender, wave_index,latent=0,init_age=65,init_state=0,n_sim=100,return_expected=1)
###
simulated_individual_path=health5_simulate_paths(trans_probs_5,init_age,init_state=0,cohort = 10000)
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

### future life time given in M state
simulated_individual_path=health5_simulate_paths(trans_probs_5,init_age,init_state=1,cohort = 10000)
total_time_alive=health5_total_time_stats(simulated_individual_path, 4)
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive

##### test static model #####
trans_rates=health5_get_trans_rates(model='S', params, init_age, gender, wave_index, latent)
trans_probs_5_age65=health5_get_trans_probs_at_age(model='S',params, init_age, gender, wave_index, latent)
###
trans_probs_5=health5_get_trans_probs(model='S',params, init_age=65, gender, wave_index)
###
created_lifetable=health5_create_life_table(trans_probs_5,init_age=65,init_state=0)
###
simulated_individual_path=health5_simulate_paths(trans_probs_5,init_age,init_state=0,cohort = 10000)
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

##### test trend model #####
trans_rates=health5_get_trans_rates(model='T', params, init_age, gender, wave_index, latent)
trans_probs_5_age65=health5_get_trans_probs_at_age(model='T',params, init_age, gender, wave_index, latent)
###
trans_probs_5=health5_get_trans_probs(model='T',params, init_age=65, gender, wave_index)
###
created_lifetable=health5_create_life_table(trans_probs_5,init_age=65,init_state=0)
simulated_individual_path=health5_simulate_paths(trans_probs_5,init_age,init_state=0,cohort = 10000)
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
