# first time of entering each state
first_time_M=function(simulated_path){
  first_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    first_time[i,]= which(simulated_path[i,] == 1)[1]
  }
  return(first_time)
}

first_time_MD=function(simulated_path){
  first_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    first_time[i,]= which(simulated_path[i,] == 2)[1]
  }
  return(first_time)
}

first_time_D=function(simulated_path){
  first_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    first_time[i,]= which(simulated_path[i,] == 3)[1]
  }
  return(first_time)
}

first_time_Dead=function(simulated_path){
  first_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    first_time[i,]= which(simulated_path[i,] == -1)[1]
  }
  return(first_time)
}

first_time_leave_H=function(simulated_path){
  first_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    first_time[i,]= which(simulated_path[i,] != 0)[1]
  }
  return(first_time)
}

# example to use
ftDead=first_time_Dead(simulated_path)

# total time in each state
total_time_H=function(simulated_path){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    total_time[i,]= length(which(simulated_path[i,] == 0))
  }
  return(total_time)
}

total_time_M=function(simulated_path){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    total_time[i,]= length(which(simulated_path[i,] == 1))
  }
  return(total_time)
}

total_time_MD=function(simulated_path){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    total_time[i,]= length(which(simulated_path[i,] == 2))
  }
  return(total_time)
}

total_time_D=function(simulated_path){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    total_time[i,]= length(which(simulated_path[i,] == 3))
  }
  return(total_time)
}

total_time_Dead=function(simulated_path){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    total_time[i,]= length(which(simulated_path[i,] == -1))
  }
  return(total_time)
}

total_time_Alive=function(simulated_path){
  total_time=matrix(nrow=nrow(simulated_path),ncol=1)
  for (i in 1:nrow(simulated_path)){
    total_time[i,]= length(which(simulated_path[i,] != -1))
  }
  return(total_time)
}

# example to use
total_time_H1=total_time_H(simulated_path)

total_time_M1=total_time_M(simulated_path)

total_time_MD1=total_time_MD(simulated_path)

total_time_D1=total_time_D(simulated_path)

total_time_Dead1=total_time_Dead(simulated_path)

total_time_Alive1=total_time_Alive(simulated_path)

# test result, result should be 47
mean(total_time_H1, na.rm = TRUE)+mean(total_time_M1, na.rm = TRUE)+mean(total_time_MD1, na.rm = TRUE)+mean(total_time_D1, na.rm = TRUE)+mean(total_time_Dead1, na.rm = TRUE)

# produce stats
stats_produce=function(input){
  output=matrix(nrow=1, ncol = 2)
  colnames(output) <- c('expected_value', 'st_dev')
  output[1]=mean(input, na.rm = TRUE)
  output[2]=sd(input, na.rm = TRUE)
  return(output)
}

# example to use
stats_first_time_leave_H=stats_produce(first_time_leave_H(simulated_path))
stats_first_time_leave_H

stats_first_time_enter_M=stats_produce(first_time_M(simulated_path))
stats_first_time_enter_M

stats_first_time_enter_MD=stats_produce(first_time_MD(simulated_path))
stats_first_time_enter_MD

stats_first_time_enter_D=stats_produce(first_time_D(simulated_path))
stats_first_time_enter_D

stats_future_life_time=stats_produce(total_time_Alive(simulated_path))
stats_future_life_time

