library(devtools)
load_all(export_all=FALSE)

params=US_HRS_5
model='F'

init_age=65
gender=0
wave_index=8 # wave index
latent=0 # initial value of latent factor

trans_probs_5=get_trans_probs(n_states=5, model_type='F', param_file='US_HRS_5', age=65, gender=0, year = 2012, wave_index = 8, latent = 0)
