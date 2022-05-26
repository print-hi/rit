#' the function to get a full list of transition probability matrices from the initial age to age 110
#'
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param init_age
#' the initial age of the transition probability matrices
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#'
#' @return a list of 5 times 5 transition probability matrices, from the initial age to age 110
#' @import readxl expm
#' @export
#'
#' @examples
#' trans_prob_matrix_age65to110=get_full_trans_prob_matrix_5(params, init_age=65, gender, i, model=3)
get_full_trans_prob_matrix_5=function(params, init_age, gender, i, model){
latent=0 # initial value of latent factor
# list of 46 vectors of transition rates for this simulation
trans_rate=list()
# list of 46 matrices of transition probabilities for this simulation
trans_prob_matrix=list()
#list of
state_status=matrix(nrow = 110-init_age+2, ncol = 5)
state_status[1,]=c(1,0,0,0,0) # initial state status is 1 in the healthy state and 0 for the others
#  for (age in 65:110){
#    state_status[[age-63]]=cbind(c(0,0,0,0,0)) # construct the list to be filled later
#  }

for (a in init_age:110){
  trans_prob_matrix[[a-init_age+1]]=transition_probability_5(params,a,gender,i+(a-init_age)/2,latent, model) # calculate transition probability matrix for each age
  if (model==3){
    latent=latent+rnorm(1) # simulate the latent factor
  }
  average_time_state=colSums(state_status) # the order is H M D MD Dead
}
return(trans_prob_matrix)
}





