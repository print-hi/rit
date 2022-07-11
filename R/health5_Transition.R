#' function to calculate transition rates at a certain age
#'
#' @param params
#' matrix of estimated parameters to construct the five state model. The rows are beta, gamma_age,
#' gamma_f, phi (if trend or frailty model), alpha (if frailty model). The columns are 1-12 transition types.
#' @param age
#' age of the individual
#' @param gender
#' gender 1 if female, 0 if male
#' @param wave_index
#' the wave index
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param model
#' S for static model, T for trend model, F for frailty model
#' @return
#' 12 times 1 vector of transition rates for the 12 types of transitions
#' @export health5_get_trans_rates
#'
#' @examples
#' transition_rates=health5_get_trans_rates(model='F',params=params_5_frailty, age=65, gender=0, wave_index=8, latent=0)
health5_get_trans_rates=function(model,params,age,gender,wave_index,latent){
    #
    if (model == 'S') {
        params <- params[1:3, 3:14]
    } else if (model == 'T') {
        params <- params[6:9, 3:14]
    } else if (model == 'F') {
        params <- params[11:15, 3:14]
    }
    #
    if (model== 'S'){
        vari_x=matrix(c(1,age,gender),ncol=1) # construct a column vector of the variables
        vari_x1=vari_x+cbind(c(0,1,0))
    }
    if (model== 'T'){
        vari_x=matrix(c(1,age,gender,wave_index),ncol=1) # construct a column vector of the variables
        vari_x1=vari_x+cbind(c(0,1,0,0))
    }
    if (model== 'F'){
        vari_x=matrix(c(1,age,gender,wave_index,latent),ncol=1) # construct a column vector of the variables
        vari_x1=vari_x+cbind(c(0,1,0,0,0))
    }
    ln_trans_rate_x=t(params)%*%vari_x # do matrix calculation to get a column vector of the ln transition rates
    ln_trans_rate_x1=t(params)%*%vari_x1

    integral_x=exp(ln_trans_rate_x)/params[2,] # use integration to approximate the midpoint rates
    integral_x1=exp(ln_trans_rate_x1)/params[2,]

    trans_rate=t(exp(ln_trans_rate_x1)/params[2,]-exp(ln_trans_rate_x)/params[2,]) # column vector of transition rates
    return(trans_rate)
}

#' function to get a matrix of transition probabilities at a certain age
#'
#' @param params
#' matrix of estimated parameters to construct the five state model. The rows are beta, gamma_age,
#' gamma_f, phi (if trend or frailty model), alpha (if frailty model). The columns are 1-12 transition types.
#' @param age
#' age of the individual
#' @param gender
#' gender 1 if female, 0 if male
#' @param wave_index
#' the wave index
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param model
#' S for static model, T for trend model, F for frailty model
#'
#' @return
#' 5 times 5 matrix of transitions probabilities, the states are H M D MD Dead on the rows and columns
#' @export health5_get_trans_probs
#' @import expm
#'
#' @examples
#' transition_probabilities=health5_get_trans_probs(model='F', params=params_5_frailty, age=65, gender=0, wave_index=8, latent=0)
health5_get_trans_probs=function(model,params,age,gender,wave_index,latent){
    trans_rate=health5_get_trans_rates(model,params,age,gender,wave_index,latent)
    if (model == 'S') {
        params <- params[1:3, 3:14]
    } else if (model == 'T') {
        params <- params[6:9, 3:14]
    } else if (model == 'F') {
        params <- params[11:15, 3:14]
    }
    trans_rate_matrix=rbind(c(-sum(trans_rate[1:4]),trans_rate[1],trans_rate[2],trans_rate[3],trans_rate[4]),
                            c(0,-sum(trans_rate[5:6]),0,trans_rate[5],trans_rate[6]),
                            c(trans_rate[7],trans_rate[8],-sum(trans_rate[c(7,8,9,10)]),trans_rate[9],trans_rate[10]),
                            c(0,trans_rate[11],0,-sum(trans_rate[11:12]),trans_rate[12]),
                            c(0,0,0,0,0))
    trans_prob_matrix=expm(trans_rate_matrix)
    return(trans_prob_matrix)
}


#' the function to get a full list of transition probability matrices from the initial age to age 110
#'
#' @param params
#' matrix of estimated parameters to construct the five state model. The rows are beta, gamma_age,
#' gamma_f, phi (if trend or frailty model), alpha (if frailty model). The columns are 1-12 transition types.
#' @param init_age
#' the initial age of the transition probability matrices
#' @param gender
#' gender 1 if female, 0 if male
#' @param wave_index
#' the wave index
#' @param model
#' S for static model, T for trend model, F for frailty model
#'
#' @return a list of 5 times 5 transition probability matrices, from the initial age to age 110
#' @import readxl expm
#' @export health5_get_list_trans_prob_matrix
#'
#' @examples
#' trans_prob_matrix_age65to110=health5_get_list_trans_prob_matrix(model='F', params=params_5_frailty, init_age=65, gender=0, wave_index=8)
health5_get_list_trans_prob_matrix=function(model, params, init_age, gender, wave_index){
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
        trans_prob_matrix[[a-init_age+1]]=health5_get_trans_probs(model, params,a,gender,wave_index+(a-init_age)/2,latent) # calculate transition probability matrix for each age
        if (model==3){
            latent=latent+rnorm(1,0,sqrt(0.5)) # simulate the latent factor
        }
        average_time_state=colSums(state_status) # the order is H M D MD Dead
    }
    return(trans_prob_matrix)
}
