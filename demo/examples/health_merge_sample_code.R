library(devtools)
load_all(export_all=FALSE)

param_file=US_HRS_5
model_type='F'

init_age=65
female=0
wave_index=8 # wave index
latent=0 # initial value of latent factor

n_states=5

trans_probs_5state=get_trans_probs(n_states, model_type, param_file, init_age, female, year = 2012, wave_index = 8, latent = 0)

lifetable_created_5state=create_life_table(trans_probs=trans_probs_5state, init_age, init_state = 0, cohort = 1)

lifetable_simulated_5state=simulate_life_table(n_states, model_type, param_file, init_age, female, year = 2012, init_state = 0, wave_index = 8,latent=0,n_sim=100,cohort=1,mean=FALSE)
mean_lifetable_simulated_5state=simulate_life_table(n_states, model_type, param_file, init_age, female, year = 2012, init_state = 0, wave_index = 8,latent=0,n_sim=100,cohort=1,mean=TRUE)

simulated_paths_5state=simulate_health_state_paths(trans_probs=trans_probs_5state, init_age, init_state = 0, cohort = 100000)
###
first_time_H=health5_first_time_stats(simulated_paths_5state, 0)
total_time_alive=health5_total_time_stats(simulated_paths_5state, 4)
###
stats_first_time_H=health5_stats_produce(first_time_H)
stats_first_time_H
### future life time given in H state
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive
###
total_time_1=health5_total_time_stats(simulated_paths_5state, 0)
total_time_2=health5_total_time_stats(simulated_paths_5state, 1)
total_time_3=health5_total_time_stats(simulated_paths_5state, 2)
total_time_4=health5_total_time_stats(simulated_paths_5state, 3)
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
simulated_paths_5state=simulate_health_state_paths(trans_probs=trans_probs_5state, init_age, init_state = 1, cohort = 100000)
total_time_alive=health5_total_time_stats(simulated_paths_5state, 4)
stats_total_time_alive=health5_stats_produce(total_time_alive)
stats_total_time_alive
