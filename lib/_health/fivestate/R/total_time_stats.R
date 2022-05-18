#' function to get the total time in different states for a number of individuals
#' @param simulated_path
#' the simulated path of individuals from the function simulate_individual_path
#' @param state
#' 0 for total time in H state
#' 1 for total time in M state
#' 2 for total time in D state
#' 3 for total time in MD state
#' -1 for total time in dead state
#' 4 for total time alive or not in dead state
#'
#' @return
#' a column that consists the total time in different states for a number of individuals
#' @export
#'
#' @examples
#' total_time_alive=total_time_stats(simulated_individual_path, 4)
total_time_stats=function(simulated_path, state){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  if (state==0){
    for (i in 1:nrow(simulated_path)){
      if (simulated_path[i,1]==0){
      total_time[i,]= length(which(simulated_path[i,] == 0))-0.5
      }else{
      total_time[i,]= length(which(simulated_path[i,] == 0))
      }
    }
  }
  if(state==1){
    for (i in 1:nrow(simulated_path)){
      if (simulated_path[i,1]==1){
      total_time[i,]= length(which(simulated_path[i,] == 1))-0.5
      }else{
      total_time[i,]= length(which(simulated_path[i,] == 1))
      }
    }
  }
  if(state==2){
    for (i in 1:nrow(simulated_path)){
      if (simulated_path[i,1]==2){
      total_time[i,]= length(which(simulated_path[i,] == 2))-0.5
      }else{
      total_time[i,]= length(which(simulated_path[i,] == 2))
      }
    }
  }
  if(state==3){
    total_time=matrix(nrow=nrow(simulated_path),ncol=1)
    for (i in 1:nrow(simulated_path)){
      if (simulated_path[i,1]==3){
      total_time[i,]= length(which(simulated_path[i,] == 3))-0.5
      }else{
      total_time[i,]= length(which(simulated_path[i,] == 3))
      }
    }
  }
  if(state==-1){
    for (i in 1:nrow(simulated_path)){
      if (simulated_path[i,1]==-1){
      total_time[i,]= length(which(simulated_path[i,] == -1))-1
      }else{
      total_time[i,]= length(which(simulated_path[i,] == -1))-0.5
      }
    }
  }
  if(state==4){
    for (i in 1:nrow(simulated_path)){
      if (simulated_path[i,1]==-1){
      total_time[i,]= length(which(simulated_path[i,] != -1))
      }else{
      total_time[i,]= length(which(simulated_path[i,] != -1))-0.5
      }
    }
  }
return(total_time)
}


