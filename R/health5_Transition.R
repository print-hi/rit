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
#' @export get_full_trans_prob_matrix
#'
#' @examples
#' trans_prob_matrix_age65to110=get_full_trans_prob_matrix(params, init_age=65, gender, i, model=3)
get_full_trans_prob_matrix=function(params, init_age, gender, i, model){
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
  trans_prob_matrix[[a-init_age+1]]=transition_probability_5_frailty(params,a,gender,i+(a-init_age)/2,latent, model) # calculate transition probability matrix for each age
  if (model==3){
    latent=latent+rnorm(1) # simulate the latent factor
  }
  average_time_state=colSums(state_status) # the order is H M D MD Dead
}
return(trans_prob_matrix)
}


#' function to get a matrix of transition probabilities at a certain age
#'
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param age
#' age of the individual
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#'
#' @return
#' 5 times 5 matrix of transitions probabilities, the states are H M D MD Dead on the rows and columns
#' @export transition_probability_5_frailty
#' @import expm
#'
#' @examples
#' transition_probabilities=transition_probability_5_frailty(params, init_age, gender, i, latent, model=3)
transition_probability_5_frailty=function(params,age,gender,i,latent,model){
  trans_rate=transition_rate_5_frailty(params,age,gender,i,latent,model)
  trans_rate_matrix=rbind(c(-sum(trans_rate[1:4]),trans_rate[1],trans_rate[2],trans_rate[3],trans_rate[4]),
                          c(0,-sum(trans_rate[5:6]),0,trans_rate[5],trans_rate[6]),
                          c(trans_rate[7],trans_rate[8],-sum(trans_rate[c(7,8,9,10)]),trans_rate[9],trans_rate[10]),
                          c(0,trans_rate[11],0,-sum(trans_rate[11:12]),trans_rate[12]),
                          c(0,0,0,0,0))
  trans_prob_matrix=expm(trans_rate_matrix)
  return(trans_prob_matrix)
}

#' function to calculate transition rates at a certain age
#'
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param age
#' age of the individual
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#' @return
#' 12 times 1 vector of transition rates for the 12 types of transitions
#' @export transition_rate_5_frailty
#'
#' @examples
#' transition_rates=transition_rate_5_frailty(params, init_age, gender, i, latent, model=3)
transition_rate_5_frailty=function(params,age,gender,i,latent,model){
  if (model==1){
    vari_x=matrix(c(1,age,gender),ncol=1) # construct a column vector of the variables
    vari_x1=vari_x+cbind(c(0,1,0))
  }
  if (model==2){
    vari_x=matrix(c(1,age,gender,i),ncol=1) # construct a column vector of the variables
    vari_x1=vari_x+cbind(c(0,1,0,0))
  }
  if (model==3){
    vari_x=matrix(c(1,age,gender,i,latent),ncol=1) # construct a column vector of the variables
    vari_x1=vari_x+cbind(c(0,1,0,0,0))
  }
  ln_trans_rate_x=t(params)%*%vari_x # do matrix calculation to get a column vector of the ln transition rates
  ln_trans_rate_x1=t(params)%*%vari_x1

  integral_x=exp(ln_trans_rate_x)/params[2,] # use integration to approximate the midpoint rates
  integral_x1=exp(ln_trans_rate_x1)/params[2,]

  trans_rate=t(exp(ln_trans_rate_x1)/params[2,]-exp(ln_trans_rate_x)/params[2,]) # column vector of transition rates
  return(trans_rate)
}


