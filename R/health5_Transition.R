#' function to calculate transition rates at a certain age
#'
#' @param model_type
#' S for static model, T for trend model, F for frailty model
#' @param param_file
#' matrix of estimated parameters to construct the five state model. Generally,
#' use US_HRS_5 for 5 state model.
#' @param age
#' age of the individual
#' @param female
#' female 1 if female, 0 if male
#' @param wave_index
#' the wave index = (interview year - 1998)/2 + 1
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @return
#' 12 times 1 vector of transition rates for the 12 types of transitions
#' @export
#'
#' @examples example
health5_get_trans_rates=function(model_type,param_file,age,female,wave_index,latent){
    #
    if (model_type == 'S') {
        param_file <- param_file[1:3, 3:14]
    } else if (model_type == 'T') {
        param_file <- param_file[6:9, 3:14]
    } else if (model_type == 'F') {
        param_file <- param_file[11:15, 3:14]
    }
    #
    if (model_type== 'S'){
        # construct a column vector of the variables
        vari_x=matrix(c(1,age,female),ncol=1)
        vari_x1=vari_x+cbind(c(0,1,0))
    }
    if (model_type== 'T'){
        # construct a column vector of the variables
        vari_x=matrix(c(1,age,female,wave_index),ncol=1)
        vari_x1=vari_x+cbind(c(0,1,0,0))
    }
    if (model_type== 'F'){
        # construct a column vector of the variables
        vari_x=matrix(c(1,age,female,wave_index,latent),ncol=1)
        vari_x1=vari_x+cbind(c(0,1,0,0,0))
    }
    # do matrix calculation to get a column vector of the ln transition rates
    ln_trans_rate_x=t(param_file)%*%vari_x
    ln_trans_rate_x1=t(param_file)%*%vari_x1

    # use integration to approximate the midpoint rates
    integral_x=exp(ln_trans_rate_x)/param_file[2,]
    integral_x1=exp(ln_trans_rate_x1)/param_file[2,]

    # column vector of transition rates
    trans_rate=t(exp(ln_trans_rate_x1)/param_file[2,]-exp(ln_trans_rate_x)/param_file[2,])
    return(trans_rate)
}

#' function to get a matrix of transition probabilities at a certain age
#'
#' @param model_type
#' S for static model, T for trend model, F for frailty model
#' @param param_file
#' matrix of estimated parameters to construct the five state model. Generally,
#' use US_HRS_5 for 5 state model.
#' @param age
#' age of the individual
#' @param female
#' female 1 if female, 0 if male
#' @param wave_index
#' the wave index = (interview year - 1998)/2 + 1
#' @param latent
#' initial value of latent factor, normally take the value 0
#'
#' @return
#' 5 times 5 matrix of transitions probabilities, the states are H M D MD Dead
#' on the rows and columns
#' @export
#' @import expm
#'
#' @examples example
health5_get_trans_probs_at_age=function(model_type,param_file,age,female,wave_index,latent){
    trans_rate=health5_get_trans_rates(model_type,param_file,age,female,wave_index,latent)
    if (model_type == 'S') {
        param_file <- param_file[1:3, 3:14]
    } else if (model_type == 'T') {
        param_file <- param_file[6:9, 3:14]
    } else if (model_type == 'F') {
        param_file <- param_file[11:15, 3:14]
    }
    trans_rate_matrix=rbind(c(-sum(trans_rate[1:4]),trans_rate[1],trans_rate[2],trans_rate[3],trans_rate[4]),
                            c(0,-sum(trans_rate[5:6]),0,trans_rate[5],trans_rate[6]),
                            c(trans_rate[7],trans_rate[8],-sum(trans_rate[c(7,8,9,10)]),trans_rate[9],trans_rate[10]),
                            c(0,trans_rate[11],0,-sum(trans_rate[11:12]),trans_rate[12]),
                            c(0,0,0,0,0))
    trans_prob_matrix=expm::expm(trans_rate_matrix)
    return(trans_prob_matrix)
}


#' the function to get a full list of transition probability matrices from the
#' initial age to age 110
#'
#' @param model_type
#' S for static model, T for trend model, F for frailty model
#' @param param_file
#' matrix of estimated parameters to construct the five state model. Generally,
#' use US_HRS_5 for 5 state model.
#' @param init_age
#' the initial age of the transition probability matrices
#' @param female
#' female 1 if female, 0 if male
#' @param wave_index
#' the wave index
#' @param latent
#' initial value of latent factor, normally take the value 0
#'
#' @return a list of 5 times 5 transition probability matrices, from the
#' initial age to age 110
#' @import readxl expm
#' @export
#'
#' @examples example
health5_get_trans_probs=function(model_type, param_file, init_age, female, wave_index, latent){
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
        # calculate transition probability matrix for each age
        trans_prob_matrix[[a-init_age+1]]=health5_get_trans_probs_at_age(model_type,param_file,a,female,wave_index+(a-init_age)/2,latent)
        if (model_type=='F'){
            latent=latent+stats::rnorm(1,0,sqrt(0.5)) # simulate the latent factor
        }
        average_time_state=colSums(state_status) # the order is H M D MD Dead
    }
    return(trans_prob_matrix)
}
