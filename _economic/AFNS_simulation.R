#' get_zcp_simulation
#' 
#' Returns the simulated paths of the zero-coupon interest rate term structure. The model is based on an Arbitrage-Free Nelson Siegel model. This function uses the package `MASS` for random number generation. 
#'
#' @param num_years Number of years to forecast, counting from 2021-06-01. Default is 5 years. 
#' @param num_paths Number of simulation paths. Default is 100 paths. 
#' @param frequency One of "year", "quarter", and "month". Default is "month". Arithmetic average will be used for "year" and "quarter".
#'
#' @return A list containing 40 data frames for the simulated trajectories for maturities from 1 quarter up to 10 years. 
#' @export
#'
#' @examples sim = get_zcp_simulations(num_years = 10, num_paths = 100, frequency = "year"). To obtain all trajectories of Australia 3-month zero-coupon yields, type sim$maturity_1qtrs. 
#' 
get_zcp_simulation = function (num_years = 5, num_paths = 100, frequency = "month") {
    ################
    # error messages
    is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (num_years <= 0 | num_paths <= 0 | !is.wholenumber(num_years) | !is.wholenumber(num_paths)) {
        stop("Number of years and paths to simulate must be positivie integers. ")
        
    } else if (!frequency %in% c("year", "quarter", "month")) { 
        stop ("Frequency must be one of 'year', 'quarter', and 'month'. ") 

    } else {
        
        ##################
        # initialisation #
        ##################
        
        init_qtr = as.Date("2021-06-01")
        if (frequency == "year") {
            h = 1
            Q_est = diag(c(0.0000503249, 0.0000615258, 0.000109023))
        } else if (frequency == "quarter") {
            h = 1/4
            Q_est = diag(c(0.0000147503, 0.0000259462, 0.0000405344))
        } else {
            h = 1/12
            Q_est = diag(c(5.10007e-06, 9.88515e-06, 0.0000148928))
        }
        num_pred = num_years / h
        time_index = seq(from = init_qtr, length.out = num_pred+1, by = frequency)[-1]
        path_index = paste("trajectory_", 1:num_paths, sep = "")
        init_xt = as.numeric(c(0.0431174, -0.0453477, -0.0422812))
        init_zcp = c(-0.0000666667,  0.0003857143,  0.0005095238,  0.0003571429,  0.0001000000, -0.0001000000, -0.0001571429, -0.0000476190,  0.0002238095,  0.0006333333,  0.0011523810,  0.0017523810,  0.0024047619,  0.0031000000, 0.0038142857,  0.0045238095,  0.0052428571,  0.0059571429,  0.0066380952,  0.0073000000,  0.0079523810, 0.0085809524,  0.0091952381,  0.0097761905,  0.0103476190,  0.0109047619,  0.0114190476,  0.0119380952, 0.0124333333,  0.0129095238,  0.0133714286,  0.0138142857,  0.0142476190,  0.0146666667,  0.0150857143, 0.0154714286,  0.0158523810,  0.0162238095,  0.0165952381,  0.0169333333)
        mat_qtrs = paste(paste("maturity",1:40,sep = "_"),"qtrs",sep = "")
        
        ########################
        # estimated parameters #
        ########################
        
        KP_est = diag(c(0.222361,0.840951,0.6043))
        # Sigma_est = diag(c(0.00789569, 0.0112751, 0.0137063))
        theta_est = matrix(c(0.0404048, -0.0247579, 0.000506779), nrow = 3, ncol = 1, byrow = T)
        lambda_est = 0.323143
        B_fitted = matrix(c(1, 0.960673, 0.0382820,
                            1, 0.923395, 0.0725893,
                            1, 0.888045, 0.1032697,
                            1, 0.854512, 0.1306420,
                            1, 0.822690, 0.1549988,
                            1, 0.792481, 0.1766080,
                            1, 0.763791, 0.1957154,
                            1, 0.736534, 0.2125460,
                            1, 0.710628, 0.2273062,
                            1, 0.685997, 0.2401849,
                            1, 0.662568, 0.2513549,
                            1, 0.640274, 0.2609748,
                            1, 0.619052, 0.2691893,
                            1, 0.598841, 0.2761311,
                            1, 0.579586, 0.2819214,
                            1, 0.561235, 0.2866711,
                            1, 0.543736, 0.2904815,
                            1, 0.527045, 0.2934451,
                            1, 0.511117, 0.2956465,
                            1, 0.495911, 0.2971629,
                            1, 0.481388, 0.2980647,
                            1, 0.467512, 0.2984161,
                            1, 0.454249, 0.2982757,
                            1, 0.441565, 0.2976968,
                            1, 0.429431, 0.2967281,
                            1, 0.417817, 0.2954136,
                            1, 0.406698, 0.2937937,
                            1, 0.396047, 0.2919048,
                            1, 0.385840, 0.2897802,
                            1, 0.376054, 0.2874500,
                            1, 0.366669, 0.2849415,
                            1, 0.357665, 0.2822794,
                            1, 0.349021, 0.2794862,
                            1, 0.340720, 0.2765821,
                            1, 0.332746, 0.2735853,
                            1, 0.325081, 0.2705125,
                            1, 0.317712, 0.2673784,
                            1, 0.310624, 0.2641964,
                            1, 0.303803, 0.2609785,
                            1, 0.297236, 0.2577355), nrow = 40, byrow = T)
        A_fitted = c(1.89811e-06, 7.32411e-06, 1.59407e-05, 2.74808e-05, 4.17307e-05, 5.85172e-05, 7.76972e-05, 9.91507e-05, 1.22775e-04, 1.48478e-04, 1.76182e-04, 2.05811e-04, 2.37298e-04, 2.70581e-04,
                     3.05599e-04, 3.42296e-04, 3.80619e-04, 4.20516e-04, 4.61940e-04, 5.04845e-04, 5.49187e-04, 5.94924e-04, 6.42020e-04, 6.90436e-04, 7.40140e-04, 7.91101e-04, 8.43288e-04, 8.96676e-04,
                     9.51240e-04, 1.00696e-03, 1.06381e-03, 1.12177e-03, 1.18084e-03, 1.24099e-03, 1.30221e-03, 1.36450e-03, 1.42783e-03, 1.49221e-03, 1.55763e-03, 1.62408e-03)

        ###############
        # simulate Xt # 
        ###############
        
        Xt_indep_sim = replicate(n = num_paths,
                                 expr = {data.frame(matrix(NA, nrow = 3, ncol = num_pred+1))},
                                 simplify = F)
        Xt_indep_sim = lapply(Xt_indep_sim, function (x) {x[,1] = init_xt; return (x)})

        noise = MASS::mvrnorm(num_paths*num_pred, mu = c(0,0,0), Sigma = Q_est)
        noise_ind = 1
        for (path in 1:num_paths) { 
            for (i in 2:(num_pred+1)) {
                eta = as.matrix(noise[noise_ind,]) #as.matrix(mvrnorm(mu = c(0,0,0), Sigma = Q_est))
                Xt = as.matrix(Xt_indep_sim[[path]][,i-1])
                Xt_indep_sim[[path]][,i] = (diag(3) - expm(-h * KP_est)) %*% theta_est + expm(-h * KP_est) %*% Xt + eta
                noise_ind = noise_ind + 1
            }
        }
        
        ##############
        # compute yt # 
        ##############
        
        zcp_indep_sim = replicate(n = num_paths,
                                  expr = {matrix(NA, nrow = num_pred+1, ncol = 40)},
                                  simplify = F)
        zcp_indep_sim = lapply(1:num_paths, function (x) {zcp_indep_sim[[x]] = t(B_fitted %*% as.matrix(Xt_indep_sim[[x]])) - A_fitted})
        zcp_indep_sim = lapply(zcp_indep_sim, function(x){ x = as.data.frame(x)[-1,]; return (x)})
        zcp_indep_sim = lapply(zcp_indep_sim, function(x){row.names(x) = as.character(time_index); colnames(x) = mat_qtrs; return (x)})
    
        ###################
        # reorganise data # 
        ###################
        
        zcp_sim = zcp_indep_sim
        zcp_sim = lapply(1:40, function (x) {lapply(1:num_paths, function (y) {zcp_indep_sim[[y]][,x]})})
        zcp_sim = lapply(zcp_sim, function(x){x = as.data.frame(x)})
        zcp_sim = lapply(zcp_sim, function(x){row.names(x) = as.character(time_index); colnames(x) = path_index; return (x)})
        names(zcp_sim) = mat_qtrs
        
        return (zcp_sim)
    } 
} 