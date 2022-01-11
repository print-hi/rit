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