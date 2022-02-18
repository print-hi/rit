#' the function to get n_sim number of lifetables
#'
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param age
#' initial age of the lifetable
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param n_sim
#' the number of simulations
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#'
#' @return
#' a list of n_sum number of life table matrices
#' for each matrix, the row represents the age from the input initial age to 110, and the columns are states H M D MD Dead
#' for model 3 the frailty model, it simulates the latent factor to get n_sim number of lifetables, so we can get a distribution of the elements in the lifetable
#' for model 1 and 2, n_sim is suggest to set to be 1 to get one lifetable, otherwise it will produce the same lifetable n_sim times
#' @export
#'
#' @examples
#' simulated_lifetable=simulate_life_table(params,age,gender,i,latent,n_sim=100, model=3)
simulate_life_table=function(params,age,gender,i,latent,n_sim=100, model){
  state_status_full=list() # full list of state status for all n simulations
  expected_time_state_full=c()
  for (n in 1:100){ # 1000 simulations
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
      trans_rate[[age-64]]=transition_rate_5_frailty(params,age,gender,i,latent, model) # calculate transition rate for each age

      trans_prob_matrix[[age-64]]=transition_probability_5_frailty(params,age,gender,i,latent, model) # calculate transition probability matrix for each age

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
      if (model==3){
        latent=latent+rnorm(1) # simulate the latent factor
      }
      expected_time_state=colSums(state_status) # the order is H M D MD Dead
    }
    state_status_full=append(state_status_full,list(state_status)) # state_status_full collects all state status at all ages and of all simulations
    expected_time_state_full=rbind(expected_time_state_full,expected_time_state)
    # the size of the lists will be large
  }
return(state_status_full)
}
