#' function to get the path of 10000 individuals
#'
#' @param init_age
#' the initial age of the path
#' @param init_state
#' the initial state of all individuals
#' @param cohort
#' the number of simulations, set to be 10000
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#'
#' @return
#' 10000 rows of individual paths in the states, 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead
#' for each row it starts from the initial age as an input, and end at age 110
#' @export simulate_individual_path_5
#'
#' @examples
#' simulated_individual_path=simulate_individual_path_5(init_age=65, init_state=0, params, gender, i, cohort = 10000, model=3)
simulate_individual_path_5 <- function(init_age, init_state, params, gender, i, cohort = 10000, model) {
    # init_state 0 for H, 1 for M, 2 for D, 3 for MD, -1 for Dead

    # create empty matrix to contain simulated population
    simulated_pop <- matrix(0, nrow = cohort, ncol = 110-init_age+2)

    # initialise all individuals
    simulated_pop[, 1] <- init_state

    trans_prob_matrix=get_full_trans_prob_matrix_5(params, init_age, gender, i, model)

    for (i in 2:ncol(simulated_pop)) {
        simulated_pop[simulated_pop[,i-1] == 0, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 0),
                                                             replace = TRUE,
                                                             prob = trans_prob_matrix[[i-1]][1, ])


        simulated_pop[simulated_pop[,i-1] == 1, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 1),
                                                             replace = TRUE,
                                                             prob = trans_prob_matrix[[i-1]][2, ])

        simulated_pop[simulated_pop[,i-1] == 2, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 2),
                                                             replace = TRUE,
                                                             prob = trans_prob_matrix[[i-1]][3, ])


        simulated_pop[simulated_pop[,i-1] == 3, i] <- sample(c(0, 1, 2, 3, -1),
                                                             sum(simulated_pop[, i-1] == 3),
                                                             replace = TRUE,
                                                             prob = trans_prob_matrix[[i-1]][4, ])

        simulated_pop[simulated_pop[,i-1] == -1, i] <- -1
    }
    return(simulated_pop)
}

#' the function to get n_sim number of lifetables
#'
#' @param params
#' the input parameters from the estimation results, read from an excel file
#' @param gender
#' gender 1 if female, 0 if male
#' @param i
#' the wave index = (interview year - 1998)/2 + 1
#' @param latent
#' initial value of latent factor, normally take the value 0
#' @param n_sim
#' the number of simulations
#' @param model
#' 1 for no-frailty model, 2 for no-frailty model with a trend, 3 for frailty model
#' @param initial_state
#' 0 for H state, 1 for M state, 2 for D state, 3 for MD state
#' @param init_age
#' the initial age of the life table
#' @return
#' a list of n_sum number of life table matrices
#' for each matrix, the row represents the age from the input initial age to 110, and the columns are states H M D MD Dead
#' for model 3 the frailty model, it simulates the latent factor to get n_sim number of lifetables, so we can get a distribution of the elements in the lifetable
#' for model 1 and 2, n_sim is suggest to set to be 1 to get one lifetable, otherwise it will produce the same lifetable n_sim times
#' @export simulate_life_table_5
#'
#' @examples
#' simulated_lifetable=simulate_life_table_5(params,init_age,gender,i,latent,initial_state,n_sim=100, model=3)
simulate_life_table_5=function(params,init_age,gender,i,latent,initial_state,n_sim=100, model){
    state_status_full=list() # full list of state status for all n simulations
    expected_time_state_full=c()
    for (n in 1:100){ # 1000 simulations
        latent=0 # initial value of latent factor
        # list of 46 vectors of transition rates for this simulation
        trans_rate=list()
        # list of 46 matrices of transition probabilities for this simulation
        trans_prob_matrix=list()
        #list of lifetables
        state_status=matrix(nrow = 110-init_age+2, ncol = 6)
        colnames(state_status) <- c("Age", "Healthy", "M", "D", "MD", "Dead")
        if (initial_state==0){
            state_status[1,]=c(init_age,1,0,0,0,0) # initial state status is 1 in the healthy state and 0 for the others
        }
        if (initial_state==1){
            state_status[1,]=c(init_age,0,1,0,0,0) # initial state status is 1 in the M state and 0 for the others
        }
        if (initial_state==2){
            state_status[1,]=c(init_age,0,0,1,0,0) # initial state status is 1 in the D state and 0 for the others
        }
        if (initial_state==3){
            state_status[1,]=c(init_age,0,0,0,1,0) # initial state status is 1 in the MD state and 0 for the others
        }

        for (age in init_age:110){

            trans_prob_matrix[[age-init_age+1]]=transition_probability_5(params,age,gender,i+(age-init_age)/2,latent, model) # calculate transition probability matrix for each age

            for (j in 2:6){
                state_status[age-init_age+2,j]=state_status[age-init_age+1,2]*trans_prob_matrix[[age-init_age+1]][1,j-1]+
                    state_status[age-init_age+1,3]*trans_prob_matrix[[age-init_age+1]][2,j-1]+
                    state_status[age-init_age+1,4]*trans_prob_matrix[[age-init_age+1]][3,j-1]+
                    state_status[age-init_age+1,5]*trans_prob_matrix[[age-init_age+1]][4,j-1]+
                    state_status[age-init_age+1,6]*trans_prob_matrix[[age-init_age+1]][5,j-1]
                # state_status is essentially the life table at each age
                # firstly times the probability/or number(if assume 10000 individuals just simply times 10000) of the state at the beginning of this year with the probability of entering state j from this state
                # then take the sum of all possible initial states of this year
            }
            state_status[age-init_age+2,1]=age+1
            if (model==3){
                latent=latent+rnorm(1) # simulate the latent factor
            }
            expected_time_state=colSums(state_status) # the order is H M D MD Dead
        }
        state_status_full=append(state_status_full,list(state_status)) # state_status_full collects all state status at all ages and of all simulations
        expected_time_state_full=rbind(expected_time_state_full,expected_time_state)
        # the size of the lists will be large
    }
    return(state_status_full)
}
