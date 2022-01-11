setwd("~/Documents/GitHub/retirement-toolkit/_health")
library("readxl")
library("expm")

# read data
my_data <- read_excel("Params_US_5_state.xlsx",col_names=TRUE)

params_5_no_frailty <- my_data[1:3,3:ncol(my_data)] # model 3
  
params_5_no_frailty_with_trend <- my_data[5:8,3:ncol(my_data)] # model 4

params_5_frailty <- my_data[10:14,3:ncol(my_data)] # model 5

# select the parameter and model
params=params_5_frailty
model=5

# input characteristics of the individual at time 0
age=65
gender=0
i=9 # wave index
latent=0 # initial value of latent factor

# OLD function to calculate transition rate (kept for testing)
transition_rate_5_frailty_old=function(params,age,gender,i,latent){
  vari=matrix(c(1,age,gender,i,latent),ncol=1) # construct a column vector of the variables
  ln_trans_rate=t(params)%*%vari # do matrix calculation to get a column vector of the ln transition rates
  trans_rate=exp(ln_trans_rate) # column vector of transition rates
  mid_point_rate=1/params
  return(trans_rate)
}

# function to calculate transition rate
transition_rate_5_frailty=function(params,age,gender,i,latent){
  vari_x=matrix(c(1,age,gender,i,latent),ncol=1) # construct a column vector of the variables
  vari_x1=vari_x+cbind(c(0,1,0,0,0))
  ln_trans_rate_x=t(params)%*%vari_x # do matrix calculation to get a column vector of the ln transition rates
  ln_trans_rate_x1=t(params)%*%vari_x1
  
  integral_x=exp(ln_trans_rate_x)/params[2,] # use integration to approximate the midpoint rates
  integral_x1=exp(ln_trans_rate_x1)/params[2,]
  
  trans_rate=t(exp(ln_trans_rate_x1)/params[2,]-exp(ln_trans_rate_x)/params[2,]) # column vector of transition rates
  return(trans_rate)
}

# TESTING
transition_rate_5_frailty_old(params,age,gender,i,latent)
transition_rate_5_frailty(params,age,gender,i,latent)

### test package expm
# abc=matrix(c(-0.7,0.2,0.5,0.3,-0.3,0.3,0.4,0.1,-0.8),ncol=3)
abc=rbind(c(-0.7,0.3,0.4),c(0.2,-0.3,0.1),c(0.5,0.3,-0.8))
expm(abc)

# function to build transition probability matrix
transition_probability_5_frailty=function(trans_rate){
  trans_rate_matrix=rbind(c(-sum(trans_rate[1:4]),trans_rate[1],trans_rate[2],trans_rate[3],trans_rate[4]),
                          c(0,-sum(trans_rate[5:6]),0,trans_rate[5],trans_rate[6]),
                          c(trans_rate[7],trans_rate[8],-sum(trans_rate[c(7,8,9,10)]),trans_rate[9],trans_rate[10]),
                          c(0,trans_rate[11],0,-sum(trans_rate[11:12]),trans_rate[12]),
                          c(0,0,0,0,0))
  trans_prob_matrix=expm(trans_rate_matrix)
  
  return(trans_prob_matrix)
}

# simulation section
gender=0
i=9 # wave index
latent=0 # initial value of latent factor
n=1000 # number of simulations

state_status_full=list() # full list of state status for all n simulations
average_time_state_full=c()
for (n in 1:1000){ # 1000 simulations
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
  latent=latent+rnorm(1) # simulate the latent factor
  average_time_state=colSums(state_status) # the order is H M D MD Dead
}
  state_status_full=append(state_status_full,list(state_status)) # state_status_full collects all state status at all ages and of all simulations
  average_time_state_full=rbind(average_time_state_full,average_time_state)
  # the size of this list will be large
}

###### With only one simulation
gender=0
i=9 # wave index
latent=0 # initial value of latent factor
n=1000 # number of simulations

state_status_full=list() # full list of state status for all n simulations
average_time_state_full=c()

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
  average_time_state=colSums(state_status) # the order is H M D MD Dead
 
  rowSums(lifetable)# check if the calculation is correct