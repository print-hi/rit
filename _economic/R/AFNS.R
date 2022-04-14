#' get_afns_simulation
#' 
#' Returns the simulated paths of the zero-coupon interest rate term structure. 
#' The model is based on an Arbitrage-Free Nelson-Siegel (AFNS) model. 
#' This function uses the package `MASS` for random number generation and `expm`
#'  for matrix exponential. 
#'
#'
#' @param num_years Number of years to forecast, counting from 2021-06-01. Default is 5 years. 
#' @param num_paths Number of simulation paths. Default is 10 paths. 
#' @param frequency One of "year", "quarter", and "month" (default).  
#' @param type Either "independent" (default) or "correlated". Independent-factor 
#' model assumes independence between the latent factors for interest rates. 
#' @param model Either "interest_rate" (default) or "interest_house_stock". 
#' The latter model is based on an augmented Arbitrage-Free Nelson-Siegel model, 
#' where both home value index and stock price are assumed to be dependent on interest rates. 
#'
#' @return If model is `interest_rate`, the function returns a list containing 
#' 40 data frames for the simulated trajectories for maturities from 1 quarter up to 10 years. 
#' If model is `interest_house_stock`,the function returns a list containing 42 data frames 
#' for the simulated trajectories for maturities from 1 quarter up to 10 years, 
#' as well as NSW house value indexes and S&P/ASX200 closing prices. 
#' @export get_afns_simulation
#'
#' @examples sim = get_afns_simulations(num_years = 10, num_paths = 100, 
#' frequency = "year", type = "correlated", model = "interest_rate"). To obtain trajectories of 
#' Australia 3-month zero-coupon yields, type sim$maturity_1qtrs. To obtain trajectories of 
#' S&P/ASX200 closing prices, type sim$stock_price. 
get_afns_simulation = function (num_years = 5, num_paths = 10, frequency = "month", type = "independent", model = "interest_rate") {
    
    ##################
    # error messages #
    ##################
    is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (num_years <= 0 | num_paths <= 0 | !is.wholenumber(num_years) | !is.wholenumber(num_paths)) {
        stop("Number of years and paths to simulate must be positivie integers. ")
        
    } else if (!frequency %in% c("year", "quarter", "month")) { 
        stop ("Frequency must be one of 'year', 'quarter', and 'month'. ") 

    } else if (!type %in% c("independent", "correlated")) {
        stop ("Type must be either 'independent' or 'correlated'. ") 
        
    } else if (!model %in% c("interest_rate", "interest_house_stock")) {
        stop ("Model must be either 'interest_rate' or 'interest_house_stock'. ") 
        
    }
    
    AFNS = afns_model(model, type, frequency)
    {
        init_zcp = AFNS$init_zcp
        mat_qtrs = AFNS$mat_qtrs
        num_xt = AFNS$num_xt
        num_zcp = AFNS$num_zcp
        mu0 = AFNS$mu0
        h = AFNS$h
        Q_est = AFNS$Q_est
        EK_est = AFNS$EK_est
        init_xt = AFNS$init_xt
        KP_est = AFNS$KP_est
        theta_est = AFNS$theta_est
        lambda_est = AFNS$lambda_est
        B_fitted = AFNS$B_fitted
        A_fitted = AFNS$A_fitted
    }

    ##################
    # initialisation #
    ##################
    
    init_qtr = as.Date("2021-06-01")
    path_index = paste("trajectory_", 1:num_paths, sep = "")
    
    num_pred = num_years / h
    time_index = seq(from = init_qtr, length.out = num_pred+1, by = frequency)[-1]
    
    ###############
    # simulate Xt # 
    ###############
    
    Xt_sim = replicate(n = num_paths,
                        expr = {data.frame(matrix(NA, nrow = num_xt, ncol = num_pred+1))},
                        simplify = F)
    Xt_sim = lapply(Xt_sim, function (x) {x[,1] = init_xt; return (x)})
    noise = MASS::mvrnorm(num_paths*num_pred, mu = mu0, Sigma = Q_est)
    noise_ind = 1
    for (path in 1:num_paths) {
        for (i in 2:(num_pred+1)) {
            eta = as.matrix(noise[noise_ind,]) 
            Xt = as.matrix(Xt_sim[[path]][,i-1])
            Xt_sim[[path]][,i] = EK_est %*% theta_est + expm::expm(-h * KP_est) %*% Xt + eta
            noise_ind = noise_ind + 1
        }
    }
    
    ##############
    # compute yt # 
    ##############
    
    zcp_sim = replicate(n = num_paths,
                        expr = {matrix(NA, nrow = num_pred+1, ncol = num_zcp)},
                        simplify = F)
    zcp_sim = lapply(1:num_paths, 
                     function (x) {zcp_sim[[x]] = t(B_fitted %*% as.matrix(Xt_sim[[x]])) - A_fitted})
    zcp_sim = lapply(zcp_sim, 
                     function(x){x = as.data.frame(x)[-1,]; 
                                 row.names(x) = as.character(time_index); 
                                 colnames(x) = mat_qtrs; return (x) })
    
    ###################
    # reorganise data # 
    ###################
    
    output = zcp_sim
    output = lapply(1:num_zcp, 
                    function (x) {lapply(1:num_paths, function (y) {zcp_sim[[y]][,x]})})
    output = lapply(output, 
                    function(x){x = as.data.frame(x); 
                                row.names(x) = as.character(time_index); 
                                colnames(x) = path_index; return (x)})
    names(output) = mat_qtrs
    if (model == "interest_house_stock") {
        output[[41]] = exp(output[[41]])
        output[[42]] = exp(output[[42]])
    }

    return (output)
} 
