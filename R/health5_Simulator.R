#' function to get the path of 10000 individuals
#'
#' @param init_age
#' the initial age of the path
#' @param init_state
#' the initial state of all individuals
#' @param cohort
#' the number of simulations, set to be 10000
#' @param param_file
#' matrix of estimated parameters to construct the five state model. The rows are beta, gamma_age,
#' gamma_f, phi (if trend or frailty model), alpha (if frailty model). The columns are 1-12 transition types.
#' @param female
#' female 1 if female, 0 if male
#' @param wave_index
#' the wave index
#' @param model_type
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#'
#' @return
#' 10000 rows of individual paths in the states, 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead
#' for each row it starts from the initial age as an input, and end at age 110
#' @export health5_simulate_paths
#'
#' @examples
#' simulated_individual_path=health5_simulate_paths(model_type='F', init_age=65, 
#' init_state=0, param_file=params_5_frailty, female=0, wave_index=8, cohort = 10000)
health5_simulate_paths <- function(list_trans_probs, init_age, init_state, cohort = 10000) {
    # init_state 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead

    # create empty matrix to contain simulated population
    simulated_pop <- matrix(0, nrow = cohort, ncol = 110-init_age+2)

    # initialise all individuals
    simulated_pop[, 1] <- init_state

    for (i in 2:ncol(simulated_pop)) {
        simulated_pop[simulated_pop[,i-1] == 0, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 0),
                                                             replace = TRUE,
                                                             prob = list_trans_probs[[i-1]][1, ])


        simulated_pop[simulated_pop[,i-1] == 1, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 1),
                                                             replace = TRUE,
                                                             prob = list_trans_probs[[i-1]][2, ])

        simulated_pop[simulated_pop[,i-1] == 2, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 2),
                                                             replace = TRUE,
                                                             prob = list_trans_probs[[i-1]][3, ])


        simulated_pop[simulated_pop[,i-1] == 3, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 3),
                                                             replace = TRUE,
                                                             prob = list_trans_probs[[i-1]][4, ])

        simulated_pop[simulated_pop[,i-1] == -1, i] <- -1
    }
    return(simulated_pop)
}

#' the function to get n_sim number of lifetables
#'
#' @param param_file
#' matrix of estimated parameters to construct the five state model. The rows are beta, gamma_age,
#' gamma_f, phi (if trend or frailty model), alpha (if frailty model). The columns are 1-12 transition types.
#' @param female
#' female 1 if female, 0 if male
#' @param wave_index
#' the wave index = (interview year - 1998)/2 + 1
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param n_sim
#' the number of simulations
#' @param model_type
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#' @param init_state
#' 0 for H state, 1 for M state, 2 for D state, 3 for MD state
#' @param init_age
#' the initial age of the life table
#' @return
#' a list of n_sum number of life table matrices
#' for each matrix, the row represents the age from the input initial age to 110, 
#' and the columns are states H M D MD Dead
#' for model 3 the frailty model, it simulates the latent factor to get n_sim number of lifetables, 
#' so we can get a distribution of the elements in the lifetable
#' for model 1 and 2, n_sim is suggest to set to be 1 to get one lifetable, otherwise 
#' it will produce the same lifetable n_sim times
#' @export health5_create_life_table
#'
#' @examples
#' created_lifetable=health5_create_life_table(trans_probs_5,init_age=65,init_state=0)
health5_create_life_table=function(list_trans_probs,init_age,init_state,cohort){
        # list of 46 matrices of transition probabilities for this simulation
        #list of lifetables
        state_status=matrix(nrow = 110-init_age+2, ncol = 6)
        colnames(state_status) <- c("Age", "Healthy", "M", "D", "MD", "Dead")
        if (init_state==0){
            # initial state status is 1 in the healthy state and 0 for the others
            state_status[1,]=c(init_age,1,0,0,0,0) 
        }
        if (init_state==1){
            # initial state status is 1 in the M state and 0 for the others
            state_status[1,]=c(init_age,0,1,0,0,0) 
        }
        if (init_state==2){
            # initial state status is 1 in the D state and 0 for the others
            state_status[1,]=c(init_age,0,0,1,0,0) 
        }
        if (init_state==3){
            # initial state status is 1 in the MD state and 0 for the others
            state_status[1,]=c(init_age,0,0,0,1,0) 
        }

        for (age in init_age:110){

            for (j in 2:6){
                state_status[age-init_age+2,j]=state_status[age-init_age+1,2]*list_trans_probs[[age-init_age+1]][1,j-1]+
                    state_status[age-init_age+1,3]*list_trans_probs[[age-init_age+1]][2,j-1]+
                    state_status[age-init_age+1,4]*list_trans_probs[[age-init_age+1]][3,j-1]+
                    state_status[age-init_age+1,5]*list_trans_probs[[age-init_age+1]][4,j-1]+
                    state_status[age-init_age+1,6]*list_trans_probs[[age-init_age+1]][5,j-1]
                # state_status is essentially the life table at each age
                # firstly times the probability/or number(if assume 10000 individuals just simply times 10000) 
                # of the state at the beginning of this year with the probability of entering state j from this state
                # then take the sum of all possible initial states of this year
            }
            state_status[age-init_age+2,1]=age+1
            expected_time_state=colSums(state_status) # the order is H M D MD Dead
        }
        # the size of the lists will be large
        state_status[,c(2:6)]=state_status[,c(2:6)]*cohort
    return(state_status)
}

#' the function to get n_sim number of lifetables
#'
#' @param param_file
#' matrix of estimated parameters to construct the five state model. The rows are beta, gamma_age,
#' gamma_f, phi (if trend or frailty model), alpha (if frailty model). The columns are 1-12 transition types.
#' @param female
#' female 1 if female, 0 if male
#' @param wave_index
#' the wave index = (interview year - 1998)/2 + 1
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param n_sim
#' the number of simulations
#' @param model_type
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#' @param init_state
#' 0 for H state, 1 for M state, 2 for D state, 3 for MD state
#' @param init_age
#' the initial age of the life table
#' @return
#' a list of n_sum number of life table matrices
#' for each matrix, the row represents the age from the input initial age to 110, 
#' and the columns are states H M D MD Dead
#' for model 3 the frailty model, it simulates the latent factor to get n_sim number of lifetables, 
#' so we can get a distribution of the elements in the lifetable
#' for model 1 and 2, n_sim is suggest to set to be 1 to get one lifetable, 
#' otherwise it will produce the same lifetable n_sim times
#' @export health5_simulate_life_table
#'
#' @examples
#' simulated_lifetable=health5_simulate_life_table(model_type='F', param_file, female, 
#' wave_index,latent=0,init_age=65,init_state=0,n_sim=100,return_expected=0)
health5_simulate_life_table=function(model_type, param_file, female, wave_index,latent,init_age,init_state,n_sim, cohort, mean){
    if (model_type != 'F') {
        stop('use frailty model to simulate lifetables')
    }
    state_status_full=list()
    for (i in 1: n_sim){
    list_trans_probs=health5_get_trans_probs(model_type, param_file, init_age, female, wave_index, latent)
    state_status=health5_create_life_table(list_trans_probs,init_age,init_state, cohort)
    state_status_full=append(state_status_full,list(state_status))
    }
    if (mean != TRUE){
        return(state_status_full)
    }
    if(mean == TRUE){
        return(Reduce('+', state_status_full)/n_sim)
    }
}

