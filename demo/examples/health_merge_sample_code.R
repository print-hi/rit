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

lifetable_created_5state=create_life_table(model_type, trans_probs=trans_probs_5state, init_age, init_state = 0, cohort = 100000)

lifetable_simulated_5state=simulate_life_table(model_type, param_file, female,year = 2012,wave_index = 8,latent=0,init_age,init_state=0,n_sim=100, mean=FALSE)

simulated_paths_5state=simulate_health_state_paths(trans_probs=trans_probs_5state, init_age, init_state = 0, cohort = 100000)
