get_trans_prob_matrix=function(params, init_age, gender, i){
library("readxl")
library("expm")
latent=0 # initial value of latent factor
# list of 46 vectors of transition rates for this simulation
trans_rate=list()
# list of 46 matrics of transition probabilities for this simulation
trans_prob_matrix=list()
#list of 
state_status=matrix(nrow = 110-init_age+2, ncol = 5)
state_status[1,]=c(1,0,0,0,0) # initial state status is 1 in the healthy state and 0 for the others
#  for (age in 65:110){
#    state_status[[age-63]]=cbind(c(0,0,0,0,0)) # construct the list to be filled later
#  }

for (age in init_age:110){
  trans_rate[[age-init_age+1]]=transition_rate_5_frailty(params,age,gender,i,latent) # calculate transition rate for each age
  
  trans_prob_matrix[[age-init_age+1]]=transition_probability_5_frailty(trans_rate[[age-init_age+1]]) # calculate transition probability matrix for each age

  latent=latent+rnorm(1) # simulate the latent factor
  average_time_state=colSums(state_status) # the order is H M D MD Dead
}
return(trans_prob_matrix)
}

# example to use
trans_prob_matrix=get_trans_prob_matrix(params, init_age=65, gender=0, i=9)



