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
#' @export first_time_stats_5
#'
#' @examples
#' first_time_H=first_time_stats_5(simulated_individual_path, 0)
first_time_stats_5=function(simulated_path, state){
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
#' @export total_time_stats_5
#'
#' @examples
#' total_time_alive=total_time_stats_5(simulated_individual_path, 4)
total_time_stats_5=function(simulated_path, state){
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


#' function to produce the mean and variance of a list of values
#'
#' @param input
#' the list of values to be studied
#' @return
#' mean and variance of the
#' @export stats_produce_5
#'
#' @examples
#' stats_first_time_leave_H=stats_produce_5(first_time_stats(simulated_path,0))
stats_produce_5=function(input){
    output=matrix(nrow=1, ncol = 2)
    colnames(output) <- c('expected_value', 'st_dev')
    output[1]=mean(input, na.rm = TRUE)
    output[2]=sd(input, na.rm = TRUE)
    return(output)
}
