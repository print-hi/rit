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