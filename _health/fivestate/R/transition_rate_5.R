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
#' @export
#'
#' @examples
#' transition_rates=transition_rate_5(params, init_age, gender, i, latent, model=3)
transition_rate_5=function(params,age,gender,i,latent,model){
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
