#' get_discrete_simulations
#'
#' Returns the simulated paths for various economic and financial variables: (1) Australia 3-month zero-coupon yields, (2) Australia 10-year zero-coupon spread, (3) New South Wales houses value index, (4) New South Wales houses rental yields, (5) Australian GDP, (6) Australian CPI, (7) S&P/ASX200 closing price, (8) Australian dollar trade-weighted index, (9) Australia mortgage rate, (10) New South Wales unemployment rate (in %). 
#' Simulations are based on a Vector Autoregression model. This function uses the package `zoo` to convert the frequnency units. 
#' 
#' @param num_years Number of years to forecast, counting from 2021-01-01. Default is 5 years, recommended period is less than 10 years. 
#' @param num_paths Number of simulation paths. Default is 10000 paths. 
#' @param frequency One of "year", "quarter", and "month". Default is "quarter", which is the simulation frequency for the Vector Autoregression model. Linear interpolation will be used if the required frequency is higher, whereas arithmetic average will be used if the frequency is lower.   
#' @param perc_change If the outputs are expressed in terms of percentage change. Default is FALSE 
#' @param return_sdf If the VAR-based stochastic discount factors are returned. Default is FALSE. 
#'
#' @return A list containing 10 data frames for the simulated trajectories for each economic variable, and a list of white noises in the VAR model. 
#' @export
#'
#' @examples sim = get_discrete_simulations(num_years = 10, num_paths = 100, frequency = "year", return_sdf = T). To obtain all trajectories of Australia 3-month zero-coupon yields, type sim$zcp3m_yield, to obtain the noises in the first trajectory, type sim$noise$trajectory_1. 
#' 
                                                                                        get_var_simulations = function (num_years = 5, num_paths = 100, frequency = "quarter", perc_change = FALSE, return_sdf = FALSE) {
    
    ################
    # error messages 
    is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (num_years <= 0 | num_paths <= 0 | !is.wholenumber(num_years) | !is.wholenumber(num_paths)) {
        stop("Number of years and paths to simulate must be positivie integers. ")
        
    } else if (!frequency %in% c("year", "quarter", "month")) { 
        stop ("Frequency must be one of 'year', 'quarter', and 'month'. ")
        
    } else if (!is.logical(perc_change) | !is.logical(return_sdf)) {
        stop ("perc_change and return_sdf must be logical. ")
        
    } else {
        ##########################################################
        # VAR(1) calibrated coefficients (for stationary series) #
        ##########################################################
      
        # variable names 
        var_names = c("zcp3m_yield", "zcp10y_spread", "home_index", "rental_yield", "GDP", "CPI", "ASX200", "AUD")
        sim_var_names = c(var_names, "mortgage_rate", "unemployment_rate")
        # intercept in VAR(1)
        intercept = c(-0.165665068,0.014222835,0.000697426,0.001208242,0.007404573,0.006936114,0.020845829,-0.001948064)
        # coefficient matrix in VAR(1)
        coef = data.frame(zcp3m_yield = c(0.33992941,0.113784745,2.86568168,-19.6205786,-2.1372348,0.4466213,0.99122306,0.65416550),
                          zcp10y_spread = c(-0.45145476,0.867037435,2.19363658,1.9921148,4.4229589,5.7463705,-0.56233231,1.85265166),
                          home_index = c(-0.00661822,0.000568036,0.72566350,0.0746341,0.3094571,-0.1766833,0.03114352,0.00984050),
                          rental_yield = c(0.00150427,-0.001065600,-0.00245535,0.1967380,-0.0335203,0.0482826,0.00803609,0.00148116),
                          GDP = c(-0.00243469,0.001739440,-0.06584194,0.3091203,-0.0617071,-0.1506179,0.01758549,0.04417218),
                          CPI = c(0.00256898,-0.000754316,0.00615750,-0.1858264,-0.0911666,0.0884468,0.00387475,-0.00201055),
                          ASX200 = c(-0.00167551,0.001251294,-0.32533013,-1.1531345,0.1320861,-1.9311967,0.26973160,0.12588817),
                          AUD = c(-0.02216189,0.003490000,0.26377394,-1.1205875,0.2365210,-0.7783240,-0.01069297,0.24006978),
                          row.names = c("Coeff1", "Coeff2", "Coeff3", "Coeff4", "Coeff5", "Coeff6", "Coeff7", "Coeff8"))
        coef = as.matrix(t(coef))
        # residual covariance matrix in VAR(1)
        covres = data.frame(zcp3m_yield = c(0.147782809, -0.1010170150, 2.53209e-04, -2.71426e-04, 6.60361e-04, 4.41163e-04, 0.0043253002, 7.57409e-03),
                            zcp10y_spread = c(-0.101017015, 0.2118572919, -6.23599e-05, -1.20674e-04, -4.37033e-04, -1.44520e-04, -0.0026649796, -4.78746e-03),
                            home_index = c(0.000253209, -0.0000623599, 1.43124e-04, -6.82911e-06, 8.61086e-06, -1.00958e-06, 0.0001237822, 4.30438e-05),
                            rental_yield = c(-0.000271426, -0.0001206736, -6.82911e-06, 1.67679e-05, -3.35037e-08, 2.46972e-06, -0.0000480865, -2.81226e-05),
                            GDP = c(0.000660361, -0.0004370325, 8.61086e-06, -3.35037e-08, 9.90145e-05, 2.00431e-05, 0.0001435020, 7.72833e-06),
                            CPI = c(0.000441163, -0.0001445196, -1.00958e-06, 2.46972e-06, 2.00431e-05, 3.09181e-05, 0.0000535892, 2.50834e-05),
                            ASX200 = c(0.004325300, -0.0026649796, 1.23782e-04, -4.80865e-05, 1.43502e-04, 5.35892e-05, 0.0029571153, 9.07070e-04),
                            AUD = c(0.007574089, -0.0047874552, 4.30438e-05, -2.81226e-05, 7.72833e-06, 2.50834e-05, 0.0009070698, 1.45155e-03),
                            row.names = var_names)
        covres = as.matrix(covres)
        
        #############################
        # simulation initialisation #
        #############################
        
        # starting quarter 
        init_qtr = as.Date("2021-01-01")
        num_pred = 4 * num_years 
        time_index = seq(from = init_qtr, length.out = num_pred + 1, by = "quarter")
        path_index = paste("trajectory_", 1:num_paths, sep = "")
        # initial values for stationary series 
        init_stat = c(-0.033243728,1.422580645,0.050236685, -0.000455718,0.018766511,0.005954930,0.059250269,0.037258168)
        # initial values for original series 
        init_orig = c(-0.00435484, 1.418226, 157.5923, 0.04984992, 501788, 117.9, 6766.84, 63.8000, 5.70600)
        
        ############################
        # step-by-step simulations # 
        ############################
        
        # white noise
        noise = matrix(data = rnorm(length(intercept) * num_pred * num_paths, 0, 1), nrow = length(intercept))
        noise = lapply(seq(from = 1, to = num_paths * num_pred, by = num_pred), function (x) {noise[, x:(x+num_pred-1)]})
        noise = lapply(noise, function(x) {row.names(x) = var_names; colnames(x) = as.character(time_index[-1]); return (x)})
        names(noise) = path_index
        
        # whole path (inputs/outputs are both stationary)
        var_path = function (num_pred, noise_index) {
            path = as.data.frame(matrix(NA, nrow = num_pred, ncol = length(var_names)))
            row.names(path) = time_index[-1]
            colnames(path) = var_names 
            
            # simulate for num_pred steps 
            new_init = init_stat
            for (i in 1:num_pred) {
                e = as.vector(noise[[noise_index]][,i])
                new = intercept + coef %*% new_init + as.matrix(chol(covres)) %*% e
                path[i,] = new
                new_init = new
            }
            return (path)
        }
        
        ########################################
        # simulation for the stationary series #
        ########################################
        
        var_sim_stationary = function (num_pred, num_paths) {
            
            # loops thru the series (separate lists)
            v_path = replicate(n = num_paths, 
                               expr = {data.frame(matrix(NA, nrow = num_pred, ncol = length(intercept)))},
                               simplify = F)
            v_path = lapply(1:num_paths, function (x) {var_path(num_pred, x)})
            return (v_path)
        }
        stat = var_sim_stationary(num_pred, num_paths)
        names(stat) = path_index
        
        ##################################################
        # convert forecasted variables -> original units #
        ##################################################
        
        diff_inv = function (x, init) {
          # undo differencing once: zcp3m_yield, rental_yield
          diffinv(x, xi = init)
        }
        index2grow_inv = function (x, init) {
          # reverse index to growth rate: home_value, gdp, cpi, asx200, aud
          Reduce (function (init, x) {init * exp(x)}, c(init, x), accumulate = T)
        }
        
        # adj for the original series
        sdf_use = replicate(n = num_paths, 
                         expr = {data.frame(matrix(NA, nrow = num_pred+1, ncol = length(intercept)))},
                         simplify = F)
        orig = replicate(n = num_paths, 
                            expr = {data.frame(matrix(NA, nrow = num_pred+1, ncol = length(intercept)))},
                            simplify = F)
        sdf_use = lapply(1:num_paths, function (x) {sdf_use[[x]] = cbind(diff_inv(stat[[x]][,1], init_orig[1])/100, # zcp3m_yield 
                                                                   c(init_stat[2], stat[[x]][,2])/100, # zcp10y
                                                                   c(init_stat[3], stat[[x]][,3]), # home_index
                                                                   diff_inv(stat[[x]][,4], init_orig[4]), # rental_yield 
                                                                   c(init_stat[5], stat[[x]][,5]), # GDP
                                                                   c(init_stat[6], stat[[x]][,6]), # CPI
                                                                   c(init_stat[7], stat[[x]][,7]), # ASX200
                                                                   c(init_stat[8], stat[[x]][,8]))}) # AUD

        orig = lapply(1:num_paths, function (x) {orig[[x]] = cbind(sdf_use[[x]][,1], # zcp3m_yield 
                                                                   sdf_use[[x]][,2], # zcp10y
                                                                   index2grow_inv(stat[[x]][,3], init_orig[3]), # home_index
                                                                   sdf_use[[x]][,4], # rental_yield 
                                                                   index2grow_inv(stat[[x]][,5], init_orig[5]), # GDP
                                                                   index2grow_inv(stat[[x]][,6], init_orig[6]),  # CPI
                                                                   index2grow_inv(stat[[x]][,7], init_orig[7]),  # ASX200
                                                                   index2grow_inv(stat[[x]][,8], init_orig[8]))}) # AUD
        
        # reorganise the results 
        sim = replicate(n = length(var_names), 
                         expr = {data.frame(matrix(NA, nrow = num_pred, ncol = num_paths))},
                         simplify = F)
        sim = lapply(1:length(intercept), function (y) { lapply(1:num_paths, function (x) {orig[[x]][,y]}) })
        sim = lapply(sim, function(x){x = as.data.frame(x)})
        sim = lapply(sim, function(x){row.names(x) = time_index; colnames(x) = path_index; return (x)})
        sim[[9]] = sim[[1]] + 0.02825 # mortage_rate
        sim[[10]] = sim[[2]] + 0.04956 # unemployment_rate 
        sim = lapply(sim, function (x) {x = as.data.frame(x)})
        sim = lapply(sim, function (x) {row.names(x) = time_index; x})
        names(sim) = sim_var_names
        
        
        ###############################
        # stochastic discount factors #
        ###############################
        
        if (isTRUE(return_sdf)) {
          #################################
          # market price of risk (lambda_t)

          # parameters 
          lambda_0 = matrix(c(-0.0300869, -0.012627, -0.0179271, 0.0245439, -0.0141266, -0.013161, -0.0339329, -0.0298033), nrow = 8)
          lambda_1 = matrix(c(-0.00676676,0.00190172,0.00375620,0.006151668,0.00116080,0.00081834,0.00363284,0.00510658,
                              0.00874009,0.00329734,0.00514494,0.007689653,0.00494228,0.01165728,0.00777616,0.01117729,
                              -0.00399927,0.00188649,0.00345333, -0.005177457,0.00722763, -0.00883047,0.00290525,0.01205820,
                              0.01598942,0.01589751,0.01790125,0.011338143,0.00546149,0.01048769,0.00908834,0.01011179,
                              -0.00319565,0.00488528,0.00568038, -0.001230780,0.00773241,0.00787398,0.00216374, -0.01536454,
                              -0.00164112,0.01035217,0.00172788,0.000642188, -0.00192222,0.00954098,0.00139571,0.01200787,
                              -0.02448375, -0.01474440, -0.00439583,0.049782154, -0.00573123, -0.00386753, -0.00522005,0.01668418,
                              -0.03278908, -0.00688552,0.00839877,0.002207839, -0.00159956, -0.00580826,0.00300228,0.01805823), nrow = 8, byrow = T)
          
          # find lambda_t's for different trajectories 
          lambda_t = replicate(n = num_paths, 
                               expr = {matrix(NA, ncol = num_pred+1, nrow = 8)},
                               simplify = F)
          lambda_t = lapply(1:num_paths, function (x) {lambda_t[[x]] = sapply(1:(num_pred+1),  function (y) {lambda_t[[x]][,y] = lambda_0 + lambda_1 %*% as.matrix(sdf_use[[x]][y,])})})
          lambda_t = lapply(lambda_t, function (x) {x = as.data.frame(x); row.names(x) = var_names; colnames(x) = as.character(time_index); x = x[,-ncol(x)]; return (x)})
          
          #################
          # Pricing kernels 
          
          # find s_t for different trajectories 
          st = as.data.frame(matrix(NA, nrow = num_pred, ncol = num_paths))
          init_st = 0.997953 # historical discount factor 
          st_expn = function (time,path) {
            # finds s(t+1)
            exp(-sdf_use[[path]][time,1] - 1/2 * sum(lambda_t[[path]][,time]^2) - sum(lambda_t[[path]][,time] * noise[[path]][,time]))
          }
          st = sapply(1:num_paths, function (x) {sapply(1:num_pred, function (y) {st[y,x] = st_expn(y,x)}, simplify = T)}, simplify = T)
          st = rbind(init_st,st)
          row.names(st) = as.character(time_index)
          colnames(st) = path_index
          
          ########
          # output
          sim[[length(sim_var_names) + 1]] = st
          names(sim)[length(sim_var_names) + 1] = "discount_factors"
        }
        
        #################
        # Adj frequency #
        #################
        
        output = list()
        if (frequency == "month") {
            time_index_month = seq(from = init_qtr, length.out = num_years * 12 + 1, by = "month")
            
            qtr2month = function (x) {
                # transforms quarterly data to monthly data 
                qtr_data = zoo::zoo (x, time_index)
                month_data = zoo::zoo (NA, time_index_month)
                data = merge (qtr_data, month_data)
                data$month_data = na.approx(data$qtr_data, rule=12)
                return (as.vector(data$month_data))
            }
            output = lapply(sim, function (x) apply(x, 2, qtr2month))
            output = lapply(output, function(x) { row.names(x) = as.character(time_index_month); return (x[-nrow(x), ]) })
            
        } else if (frequency == "quarter") {
            output = lapply(sim, function(x) {x = x[-nrow(x), ]}) # remove the last row (1 Jan)
            
        } else if (frequency == "year") {
            time_index_year = seq(from = init_qtr, length.out = num_years, by = "year")
            output = lapply(sim, function (x) apply(x[-nrow(x), ], 2, function (y) {colMeans(matrix(y, nrow=4))} ))
            output = lapply(output, function(x) {row.names(x) = as.character(time_index_year); x})
        }
        output = lapply(output, function(x){x = as.data.frame(x)})
        
        #############
        # Adj units # 
        #############
        
        if (isTRUE(perc_change)) {
            output = lapply(output, function (x) {(x[-1,] - x[-nrow(x),]) / x[-nrow(x), ]})
        }

        return (output)
    }
}
