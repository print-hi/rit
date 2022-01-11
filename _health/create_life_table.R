###### Create life table with only one simulation
gender=0
i=9 # wave index
latent=0 # initial value of latent factor
n=1000 # number of simulations

state_status_full=list() # full list of state status for all n simulations
expected_time_state_full=c()

latent=0 # initial value of latent factor
# list of 46 vectors of transition rates for this simulation
trans_rate=list()
# list of 46 matrics of transition probabilities for this simulation
trans_prob_matrix=list()
# list of 46 matrics of accumulated transition probabilities for this simulation
trans_sum_prob_matrix=list()
#list of 
state_status=matrix(nrow = 110-65+2, ncol = 5)
state_status[1,]=c(1,0,0,0,0) # initial state status is 1 in the healthy state and 0 for the others
#  for (age in 65:110){
#    state_status[[age-63]]=cbind(c(0,0,0,0,0)) # construct the list to be filled later
#  }
lifetable=matrix(nrow = 110-65+2, ncol = 17)
lifetable[1,]=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # initial state status is 1 in the healthy state and 0 for the others
for (age in 65:110){
  trans_rate[[age-64]]=transition_rate_5_frailty(params,age,gender,i,latent) # calculate transition rate for each age
  
  trans_prob_matrix[[age-64]]=transition_probability_5_frailty(trans_rate[[age-64]]) # calculate transition probability matrix for each age
  
  if (age > 65) {
    trans_sum_prob_matrix[[age-64]]=trans_sum_prob_matrix[[age-65]]%*%trans_prob_matrix[[age-64]] # calculate accumulated transition probability matrix up to each age
  }else{
    trans_sum_prob_matrix[[age-64]]=trans_prob_matrix[[age-64]] 
  }
  # calculate the probability to be at state j after 1 year
  for (j in 1:5){
    state_status[age-63,j]=state_status[age-64,1]*trans_prob_matrix[[age-64]][1,j]+ 
      state_status[age-64,2]*trans_prob_matrix[[age-64]][2,j]+
      state_status[age-64,3]*trans_prob_matrix[[age-64]][3,j]+
      state_status[age-64,4]*trans_prob_matrix[[age-64]][4,j]+
      state_status[age-64,5]*trans_prob_matrix[[age-64]][5,j]
    # state_status is essentially the life table at each age
    # firstly times the probability/or number(if assume 10000 individuals just simply times 10000) of the state at the beginning of this year with the probability of entering state j from this state
    # then take the sum of all possible initial states of this year
  }
  ## the variable state_status is essentially the life table, and by taking the column sum, we get the expected time spent in each state
  latent=latent+rnorm(1) # simulate the latent factor
  
  ### the more detailed life table with order HtoH HtoM HtoD HtoMD HtoDead, MtoM MtoMD MtoDead, DtoH DtoM DtoD DtoMD DtoDead, MDtoM MDtoMD MDtoDead, DeadtoDead 17 columns
  lifetable[age-63,1]=state_status[age-64,1]*trans_prob_matrix[[age-64]][1,1] # HtoH
  lifetable[age-63,2]=state_status[age-64,1]*trans_prob_matrix[[age-64]][1,2] # HtoM
  lifetable[age-63,3]=state_status[age-64,1]*trans_prob_matrix[[age-64]][1,3] # HtoD
  lifetable[age-63,4]=state_status[age-64,1]*trans_prob_matrix[[age-64]][1,4] # HtoMD
  lifetable[age-63,5]=state_status[age-64,1]*trans_prob_matrix[[age-64]][1,5] # HtoDead
  
  lifetable[age-63,6]=state_status[age-64,2]*trans_prob_matrix[[age-64]][2,2] # MtoM
  lifetable[age-63,7]=state_status[age-64,2]*trans_prob_matrix[[age-64]][2,4] # MtoMD
  lifetable[age-63,8]=state_status[age-64,2]*trans_prob_matrix[[age-64]][2,5] # MtoDead
  
  lifetable[age-63,9]=state_status[age-64,3]*trans_prob_matrix[[age-64]][3,1] # DtoH
  lifetable[age-63,10]=state_status[age-64,3]*trans_prob_matrix[[age-64]][3,2] # DtoM
  lifetable[age-63,11]=state_status[age-64,3]*trans_prob_matrix[[age-64]][3,3] # DtoD
  lifetable[age-63,12]=state_status[age-64,3]*trans_prob_matrix[[age-64]][3,4] # DtoMD
  lifetable[age-63,13]=state_status[age-64,3]*trans_prob_matrix[[age-64]][3,5] # DtoDead
  
  lifetable[age-63,14]=state_status[age-64,4]*trans_prob_matrix[[age-64]][4,2] # MDtoM
  lifetable[age-63,15]=state_status[age-64,4]*trans_prob_matrix[[age-64]][4,4] # MDtoMD
  lifetable[age-63,16]=state_status[age-64,4]*trans_prob_matrix[[age-64]][4,5] # MDtoDead
  
  lifetable[age-63,17]=state_status[age-64,5]*1 # DeadtoDead
}
## the variable state_status is essentially the life table, and by taking the column sum, we get the expected time spent in each state
expected_time_state=colSums(state_status) # the order is H M D MD Dead

rowSums(lifetable)# check if the calculation is correct, answer should be all 1