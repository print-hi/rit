#' get_discrete_simulations
#'
#' Returns the simulated paths for various economic and financial variables on a quarterly basis: (1) Australia 3-month zero-coupon yields, (2) Australia 10-year zero-coupon spread, (3) New South Wales houses value index, (4) New South Wales houses rental yields, (5) Australian GDP, (6) Australian CPI, (7) S&P/ASX200 closing price, (8) Australian dollar trade-weighted index, (9) Australia mortgage rate, (10) New South Wales unemployment rate. 
#' Simulations are based on a Vector Autoregression model. It is recommended to simulate at most 10 years. 
#' 
#' @param num_years Number of years for forecasts, counting from 2021 Quarter 2. Default is 5 years. 
#' @param num_paths Number of simulation paths. Default is 10000 paths. 
#'
#' @return Simulation trajectories. 
#' @export
#'
#' @examples get_discrete_simulations(num_years = 10, num_paths = 100)
#' 
get_discrete_simulations = function (num_years = 5, num_paths = 10000) {
    
    ################
    # error messages 
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (num_years <= 0 | num_paths <= 0 | !is.wholenumber(num_years) | !is.wholenumber(num_paths)) {
        stop("Number of years and paths to simulate must be positivie integers. ")
        
    } else {
        ########################################################
        # VAR(1) calibrated coefficients (for stationary series)
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
        
        ###########################
        # simulation initialisation
        # starting quarter 
        init_qtr = as.Date("2021-04-01")
        num_pred = 4 * num_years 
        time_index = as.yearqtr(seq(from = init_qtr, length.out = num_pred, by = "quarter"))
        path_index = paste("trajectory_", 1:num_paths, sep = "")
        # initial values for stationary series 
        init_stat = c(-0.033243728,1.422580645,0.050236685, -0.000455718,0.018766511,0.005954930,0.059250269,0.037258168)
        # initial values for original series 
        init_orig = c(-0.00435484, 1.418226, 157.5923, 0.04984992, 501788, 117.9, 6766.84, 63.8000, 5.70600)
        
        ##########################
        # step-by-step simulations 
        # white noise
        noise = matrix(data = rnorm(length(intercept) * num_pred * num_paths, 0, 1), nrow = length(intercept))
        # one-step (inputs/outputs are both stationary)
        var_step = function (initial, noise_index) {
            e = as.vector(noise[,noise_index])
            return (intercept + coef %*% initial + as.matrix(chol(covres)) %*% e)
        }
        
        # whole path (inputs/outputs are both stationary)
        var_path = function (num_pred, noise_index) {
            path = as.data.frame(matrix(NA, nrow = num_pred, ncol = length(var_names)))
            row.names(path) = time_index
            colnames(path) = var_names 
            
            # simulate for num_pred steps 
            new_init = init_stat
            for (i in 1:num_pred) {
                new = var_step(new_init, noise_index[i])
                path[i,] = new
                new_init = new
            }
            return (path)
        }
        
        ######################################
        # simulation for the stationary series 
        var_sim_stationary = function (num_pred, num_paths) {
            
            # initialises the list for output
            stat = replicate(n = length(sim_var_names), 
                             expr = {data.frame(matrix(NA, nrow = num_pred, ncol = num_paths))},
                             simplify = F)
            names(stat) = sim_var_names
            stat = lapply(stat, function(x){row.names(x) <- time_index; x})
            stat = lapply(stat, function(x){colnames(x) <- path_index; x})
            
            # loops thru the series (separate lists)
            noise_index = 1:num_pred
            for (path in 1:num_paths) {
                v_path = var_path(num_pred, noise_index)
                
                for (var in 1:length(intercept)) {
                    stat[[var]][,path] = v_path[,var]
                }
                noise_index = noise_index + num_pred
            }
            
            return (stat)
        }
        stat = var_sim_stationary(num_pred, num_paths)
        
        ################################################
        # convert forecasted variables -> original units 
        diff_inv = function (x, init) {
            # undo differencing once: zcp3m_yield, rental_yield
            diffinv(x, xi = init)[-1]
        }
        index2grow_inv = function (x, init) {
            # 1 reverse index to growth rate 
            # 2 reverse shortening once: home_value, gdp, cpi, asx200, aud
            Reduce (function (init, x) {init * exp(x)}, c(init, x), accumulate = T)[-1]
        }
        
        # simulations for the original series
        output = stat
        for (path in 1:num_paths) {
            output[[1]][,path] = diff_inv(output[[1]][,path], init_orig[1]) # zcp3m_yield 
            # zcp10y_spread: not changed 
            output[[3]][,path] = index2grow_inv(output[[3]][,path], init_orig[3]) # home_index
            output[[4]][,path] = diff_inv(output[[4]][,path], init_orig[4]) # rental_yield 
            output[[5]][,path] = index2grow_inv(output[[5]][,path], init_orig[5]) # GDP
            output[[6]][,path] = index2grow_inv(output[[6]][,path], init_orig[6]) # CPI
            output[[7]][,path] = index2grow_inv(output[[7]][,path], init_orig[7]) # ASX200
            output[[8]][,path] = index2grow_inv(output[[8]][,path], init_orig[8]) # AUD
        }
        output[[9]] = output[[1]] + 2.825 # mortage_rate
        output[[10]] = output[[2]] + 4.956 # unemployment_rate 
        
        return (output)
    }
}
