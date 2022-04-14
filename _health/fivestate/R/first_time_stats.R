#' function to get the first time leaving or entering different states for a number of individuals
#' @param simulated_path
#' the simulated path of individuals from the function simulate_individual_path
#' @param state
#' 0 for first time leaving H state, only useful when initial state is 0
#' 1 for first time entering M state
#' 2 for first time entering D state
#' 3 for first time entering MD state
#' -1 for first time entering the dead state
#'
#' @return
#' a column that consists the first time leaving or entering the state for a number of individuals
#' @export
#'
#' @examples
#' first_time_H=first_time_stats(simulated_individual_path, 0)
first_time_stats=function(simulated_path, state){
  first_time=matrix(nrow=nrow(simulated_path),ncol=1)
  if (state==0){
    for (i in 1:nrow(simulated_path)){
      first_time[i,]= which(simulated_path[i,] != 0)[1]-1.5
    }
    return(first_time)
  }
  if (state==1){
    for (i in 1:nrow(simulated_path)){
      first_time[i,]= which(simulated_path[i,] == 1)[1]-1.5
    }
  }
  if (state==2){
    for (i in 1:nrow(simulated_path)){
      first_time[i,]= which(simulated_path[i,] == 2)[1]-1.5
    }
  }
  if (state==3){
    for (i in 1:nrow(simulated_path)){
      first_time[i,]= which(simulated_path[i,] == 3)[1]-1.5
    }
  }
  if (state==-1){
    for (i in 1:nrow(simulated_path)){
      first_time[i,]= which(simulated_path[i,] == -1)[1]-1.5
    }
  }
  return(first_time)
}
