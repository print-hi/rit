#' get_sdf
#' 
#' Returns stochastic discount factors (pricing kernels) for a Vector Autoregression model that is based on ten Australian economic indicators. 
#'
#' @param num_years Number of years to forecast, counting from 2021-01-01. Default is 5 years, recommended period is less than 10 years. 
#' @param num_paths Number of simulation paths. Default is 10000 paths. 
#' @param frequency One of "year", "quarter", and "month". Default is "quarter". Linear interpolation will be used if the required frequency is "year", whereas arithmetic average will be used if the frequency is "month". 
#'
#' @return A dataframe of the stochastic discount factor trajectories. The 
#' @export
#'
#' @examples get_sdf(num_years = 10, frequency = "year"). 
#' 
get_sdf_simulation = function (num_years = 5, num_paths = 10000, frequency = "quarter") {
    
    ################
    # error messages
    is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (num_years <= 0 | num_paths <= 0 | !is.wholenumber(num_years) | !is.wholenumber(num_paths)) {
        stop("Number of years and paths to simulate must be positivie integers. ")
        
    } else if (!frequency %in% c("year", "quarter", "month")) { 
        stop ("Frequency must be one of 'year', 'quarter', and 'month'. ") 
        
    } else {
        
        ###########################
        # simulation initialisation
        init_qtr = as.Date("2021-01-01")
        num_pred = 4 * num_years 
        time_index = seq(from = init_qtr, length.out = num_pred+1, by = "quarter")
        path_index = paste("trajectory_", 1:num_paths, sep = "")
        var_names = c("zcp3m_yield", "zcp10y_spread", "home_index_growth", "rental_yield", "GDP_growth", "CPI_growth", "ASX200_growth", "AUD_growth")
        init_orig_2020q3 = c(0.0963077, 0.81692308, 146.343, 5.15003, 477277, 116.2, 6005.84, 61.7333,6.88999)
        init_orig_2020q4 = c(0.0288889, 0.86587302, 149.871, 5.03056, 492459, 117.2, 6377.55, 61.4667, 6.3657)
        
        ##############
        # VAR dynamics 
        # get VAR simulations on the original units
        zt = get_var_simulations(num_years, num_paths, return_noise = T)[-c(9,10)]
        noise = zt$noise
        zt = zt[-9]
        zt = lapply(1:8, function (x) {zt[[x]] = rbind(init_orig_2020q3[x], init_orig_2020q4[x], zt[[x]])})
        
        # transform VAR to zt units 
        index2growth = function (x) {
            x = log(x) - log(dplyr::lag(x, n = 1))
            return (x[-1])
        }
        zt[c(3,5:8)] = lapply(zt[c(3,5:8)], function (x) {x = as.data.frame(apply(x, 2, index2growth))})
        zt[-c(3,5:8)] = lapply(zt[-c(3,5:8)], function (x) {x = x[-1,]})
        zt[c(1:2,4)] = lapply(zt[c(1:2,4)], function (x) {x = x / 100})
        names(zt) = var_names
        
        # reformat 
        zt = lapply(1:num_paths, function (y) { lapply(1:8, function (x) {zt[[x]][,y]}) })
        zt = lapply(zt, function(x){x = t(as.data.frame(x)); row.names(x) = var_names; colnames(x) = c("2020-10-01", as.character(time_index[-length(time_index)])); return (x)})
        names(zt) = path_index
        
        # noise 
        noise = lapply(noise, function(x){x = rbind(rep(0,8), x); x = t(as.data.frame(x)); colnames(x) = as.character(time_index); row.names(x) = var_names; return (x)})
        
        #################################
        # market price of risk (lambda_t)
        
        # store lambda_0, lambda_1
        lambda_0 = matrix(c(-0.0300869, -0.012627, -0.0179271, 0.0245439, -0.0141266, -0.013161, -0.0339329, -0.0298033), nrow = 8)
        lambda_1 = matrix(c(-0.00676676,0.00190172,0.00375620,0.006151668,0.00116080,0.00081834,0.00363284,0.00510658,
                            0.00874009,0.00329734,0.00514494,0.007689653,0.00494228,0.01165728,0.00777616,0.01117729,
                            -0.00399927,0.00188649,0.00345333, -0.005177457,0.00722763, -0.00883047,0.00290525,0.01205820,
                            0.01598942,0.01589751,0.01790125,0.011338143,0.00546149,0.01048769,0.00908834,0.01011179,
                            -0.00319565,0.00488528,0.00568038, -0.001230780,0.00773241,0.00787398,0.00216374, -0.01536454,
                            -0.00164112,0.01035217,0.00172788,0.000642188, -0.00192222,0.00954098,0.00139571,0.01200787,
                            -0.02448375, -0.01474440, -0.00439583,0.049782154, -0.00573123, -0.00386753, -0.00522005,0.01668418,
                            -0.03278908, -0.00688552,0.00839877,0.002207839, -0.00159956, -0.00580826,0.00300228,0.01805823), nrow = 8, byrow = T)
        prem_index = paste("prem", 1:8, sep = "_")
        
        # find lambda_t's for different trajectories 
        lambda_t = replicate(n = num_paths, 
                             expr = {matrix(NA, ncol = num_pred+1, nrow = 8)},
                             simplify = F)
        lambda_t = lapply(1:num_paths, function (x) {lambda_t[[x]] = lapply(1:(num_pred+1),  function (y) {lambda_t[[x]][,y] = lambda_0 + lambda_1 %*% as.matrix(zt[[x]][,y])} )})
        lambda_t = lapply(lambda_t, function (x) {x = as.data.frame(x); row.names(x) = prem_index; colnames(x) = c(as.Date("2020-10-01"), as.character(time_index[-length(time_index)])); return (x)})
        names(lambda_t) = path_index
        
        #################
        # Pricing kernels 
        
        # find s_t for different trajectories 
        st = as.data.frame(matrix(NA, nrow = num_pred+1, ncol = num_paths))
        st_expn = function (time,path) {
            # finds s(t+1)
            exp(-zt[[path]][1,time] - 1/2 * sum(lambda_t[[path]][,time]^2) - sum(lambda_t[[path]][,time] * noise[[path]][,time]))
        }
        st = sapply(1:num_paths, function (x) {sapply(1:(num_pred+1), function (y) {st[y,x] = st_expn(y,x)}, simplify = T)}, simplify = T)
        row.names(st) = as.character(time_index)
        colnames(st) = path_index
        
        ###############
        # Adj frequency
        output = as.data.frame(st)
        
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
            output = apply(output, 2, qtr2month)
            row.names(output) = as.character(time_index_month)
            
        } else if (frequency == "quarter") {
            row.names(output) = time_index
            colnames(output) = path_index
            
        } else if (frequency == "year") {
            time_index_year = seq(from = init_qtr, length.out = num_years, by = "year")
            output = apply(output[-nrow(output), ], 2, function (y) {colMeans(matrix(y, nrow=4))} )
            row.names(output) = as.character(time_index_year)
        }
        
        return (as.data.frame(output))
    }
}
