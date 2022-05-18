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
#' @export
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
