#' esg_var_simulator
#'
#' Returns the simulated paths for various economic and financial variables:
#' (1) Australia 3-month zero-coupon yields, (2) Australia 10-year zero-coupon
#' spread, (3) New South Wales houses value index, (4) New South Wales houses
#' rental yields, (5) Australian GDP, (6) Australian CPI, (7) S&P/ASX200 closing
#' price, (8) Australian dollar trade-weighted index, (9) Australia mortgage
#' rate, (10) New South Wales unemployment rate.
#' Simulations are based on a Vector Autoregression model. This function uses
#' the package `zoo` to convert the frequnency units. Period-by-period summary statistics can be obtained from \code{esg_summary}.
#'
#' @param num_years Number of years to forecast, counting from 2021-01-01.
#' Default is 5 years, recommended period is less than 10 years.
#' @param num_paths Number of simulation paths. Default is 10 paths.
#' @param frequency One of "year", "quarter", and "month". Default is "quarter",
#' which is the simulation frequency for the Vector Autoregression model. Linear
#' interpolation will be used if the required frequency is higher, whereas
#' arithmetic average will be used if the frequency is lower.
#' @param perc_change If the outputs are expressed in terms of period-by-period percentage
#' change.Default is FALSE. The reference level, i.e., the original values in the first output period, will be appended above the percentage changes for each variable and each trajectory.
#' @param return_sdf If the VAR-based stochastic discount factors are returned.
#' Default is TRUE.
#' @param seed Specify the seed for simulations. Default is NULL.
#'
#' @return A list containing 10 data frames for the simulated trajectories for
#' each economic variable, and a list of white noises in the VAR model.
#' @export esg_var_simulator
#'
#' @examples sim = esg_var_simulator(num_years = 10, num_paths = 10,
#' frequency = "year", perc_change = FALSE, return_sdf = TRUE, seed = 2022)
esg_var_simulator = function (num_years = 5, num_paths = 10, frequency = "quarter", perc_change = FALSE, return_sdf = TRUE, seed = NULL) {

    ################
    # error messages
    is.wholenumber = function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (num_years <= 0 | num_paths <= 0 | !is.wholenumber(num_years)
        | !is.wholenumber(num_paths)) {
        stop("Number of years and paths to simulate must be positivie integers. ")

    } else if (!frequency %in% c("year", "quarter", "month")) {
        stop ("Frequency must be one of 'year', 'quarter', and 'month'. ")

    } else if (!is.logical(perc_change) | !is.logical(return_sdf)) {
        stop ("perc_change and return_sdf must be logical. ")

    }

    ##########################################################
    # VAR(2) calibrated coefficients (for stationary series) #
    ##########################################################

    # variable names
    var_names = c("zcp3m_yield", "zcp10y_spread", "home_index", "rental_yield", "GDP", "CPI", "ASX200", "AUD")
    sim_var_names = c(var_names, "mortgage_rate", "unemployment_rate")

    VAR = var_model()
    {
        intercept = VAR$intercept
        coef = VAR$coef
        covres = VAR$covres
        init_stat_2020q4 = VAR$init_stat_2020q4
        init_stat_2021q1 = VAR$init_stat_2021q1
        init_orig = VAR$init_orig
        mortgage_rate = VAR$mortgage_rate
        unemployment_rate = VAR$unemployment_rate
        lambda_0 = VAR$lambda_0
        lambda_1 = VAR$lambda_1
        init_lambdat = VAR$init_lambdat
        init_st = VAR$init_st
    }

    # intercept in VAR(1)
    # coefficient matrix in VAR(1)
    # residual covariance matrix in VAR(1)
    colnames(coef) = var_names
    colnames(covres) = var_names

    coef = as.matrix(t(coef))
    coef1 = coef
    coef1[1,1] = coef1[1,1] + 1
    coef1[4,4] = coef1[4,4] + 1
    coef2 = matrix(0,nrow = length(var_names), ncol = length(var_names))
    coef2[,1] = -coef[,1]
    coef2[,4] = -coef[,4]

    ##################
    # initialisation #
    ##################

    # starting quarter
    init_qtr = as.Date("2021-01-01")
    num_pred = 4 * num_years
    time_index = seq(from = init_qtr, length.out = num_pred + 1, by = "quarter")
    path_index = paste("trajectory_", 1:num_paths, sep = "")
    progression = floor(num_paths / 5)

    ############################
    # step-by-step simulations #
    ############################

    # white noise
    set.seed(seed)
    noise = matrix(data = stats::rnorm(length(intercept) * num_pred * num_paths, 0, 1),
                   nrow = length(intercept))
    noise = lapply(seq(from = 1, to = num_paths * num_pred, by = num_pred),
                   function (x) {noise[, x:(x+num_pred-1)]})
    noise = lapply(noise,
                   function(x) {row.names(x) = var_names;
                                colnames(x) = as.character(time_index[-1]);
                                return (x)})
    names(noise) = path_index

    # whole path (inputs/outputs are both stationary)
    var_path = function (num_pred, noise_index) {
        path = as.data.frame(matrix(NA, nrow = num_pred, ncol = length(var_names)))
        row.names(path) = time_index[-1]
        colnames(path) = var_names

        # simulate for num_pred steps
        new_init = init_stat_2021q1 # z_{t-1}
        old_init = init_stat_2020q4 # z_{t-2}

        for (i in 1:num_pred) {
            e = as.vector(noise[[noise_index]][,i])
            zt = intercept + coef1 %*% new_init + coef2 %*% old_init + as.matrix(chol(covres)) %*% e
            path[i,] = zt
            old_init = new_init # z_{t-2} <- z_{t-1}
            new_init = zt # z_{t-1} <- z_t

        }
        return (path)
    }

    ########################################
    # simulation for the stationary series #
    ########################################

    prog_ind = 1; cat("Progress: 0% \n")
    var_sim_stationary = function (num_pred, num_paths) {

        # loops thru the series (separate lists)
        v_path = replicate(n = num_paths,
                            expr = {data.frame(matrix(NA, nrow = num_pred, ncol = length(intercept)))},
                            simplify = F)
        v_path = lapply(1:num_paths,
                        function (x) {
                            if (x == progression * prog_ind && prog_ind < 5) {
                                cat(paste(20*prog_ind, "%\n", sep = ""))
                                prog_ind <<- prog_ind + 1
                            }
                            var_path(num_pred, x)

                        })
        return (v_path)
    }
    stat = var_sim_stationary(num_pred, num_paths)
    stat = lapply(stat, function (x) {cbind(x[,1:2]/100, x[,3:8])})

    ################################################
    # convert forecast variables -> original units #
    ################################################

    index2grow_inv = function (x, init) {
        # reverse index to growth rate: home_value, gdp, cpi, asx200, aud
        Reduce (function (init, x) {init * exp(x)}, c(init, x), accumulate = TRUE)
    }


    # reorganise the results
    sim = replicate(n = length(var_names),
                    expr = {data.frame(matrix(NA, nrow = num_pred, ncol = num_paths))},
                    simplify = F)
    sim = lapply(1:8, function (y) { lapply(1:num_paths, function (x) {stat[[x]][,y]}) })
    sim = lapply(sim, function (x) {as.data.frame(x)})
    sim[[1]] = rbind(init_orig[1]/100, as.data.frame(sim[[1]])) # zcp3m
    sim[[2]] = rbind(init_orig[2]/100, as.data.frame(sim[[2]])) # zcp10y
    sim[[3]] = apply(sim[[3]], 2, function (x) {index2grow_inv(x, init_orig[3])}) # home_index
    sim[[4]] = rbind(init_orig[4], as.data.frame(sim[[4]])) # rental
    sim[[5]] = apply(sim[[5]], 2, function (x) {index2grow_inv(x, init_orig[5])}) # GDP
    sim[[6]] = apply(sim[[6]], 2, function (x) {index2grow_inv(x, init_orig[6])}) # CPI
    sim[[7]] = apply(sim[[7]], 2, function (x) {index2grow_inv(x, init_orig[7])}) # ASX200
    sim[[8]] = apply(sim[[8]], 2, function (x) {index2grow_inv(x, init_orig[8])}) # AUD
    sim[[9]] = sim[[1]] + mortgage_rate # mortage_rate
    sim[[10]] = sim[[2]] + unemployment_rate # unemployment_rate
    sim = lapply(sim, function(x){row.names(x) = time_index; colnames(x) = path_index; return (x)})
    names(sim) = sim_var_names

    ###############################
    # stochastic discount factors #
    ###############################

    if (isTRUE(return_sdf)) {
        # find lambda_t's for different trajectories
        lambda_t = replicate(n = num_paths,
                                expr = {matrix(NA, ncol = num_pred+1, nrow = 8)},
                                simplify = F)
        lambda_t = lapply(1:num_paths,
                          function (x) {lambda_t[[x]] = sapply(1:num_pred,
                          function (y) {lambda_t[[x]][,y] = lambda_0 + lambda_1 %*%
                                                            t(as.matrix(stat[[x]][y,])) })})
        lambda_t = lapply(lambda_t,
                          function (x) {x = cbind(init_lambdat, as.data.frame(x));
                                        row.names(x) = var_names; colnames(x) = as.character(time_index);
                                        x = x[,-ncol(x)]; return (x)})

        # find s_t for different trajectories
        st = as.data.frame(matrix(NA, nrow = num_pred, ncol = num_paths))
        st_expn = function (time,path) {
            # finds s(t+1)
            exp(- stat[[path]][time,1] - 1/2 * sum(lambda_t[[path]][,time]^2)
                - sum(lambda_t[[path]][,time] * noise[[path]][,time]))
        }
        st = sapply(1:num_paths,
                    function (x) {sapply(1:num_pred,
                    function (y) {st[y,x] = st_expn(y,x)}, simplify = TRUE)},
                    simplify = TRUE)
        st = rbind(init_st,st)

        st[2,] = ifelse(st[2,] > 1.2,1.2,ifelse(st[2,] < 0.8, 0.8, st[2,])) # trim the irregular values: historical data: interest rate < 0
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
            data$month_data = zoo::na.approx(data$qtr_data, rule=12)
            return (as.vector(data$month_data))
        }
        output = lapply(sim,
                        function (x) apply(x, 2, qtr2month))
        output = lapply(output,
                        function(x) {row.names(x) = as.character(time_index_month);
                                     return (x[-nrow(x), ])})

    } else if (frequency == "quarter") {
        output = lapply(sim, function(x) {x = x[-nrow(x), ]}) # remove the last row (1 Jan)

    } else if (frequency == "year") {
        time_index_year = seq(from = init_qtr, length.out = num_years, by = "year")
        output = lapply(sim,
                        function (x) apply(x[-nrow(x), ], 2,
                        function (y) {colMeans(matrix(y, nrow=4))} ))
        output = lapply(output,
                        function(x) {row.names(x) = as.character(time_index_year); x})
    }
    output = lapply(output, function(x){x = t(as.data.frame(x))})

    #############
    # Adj units #
    #############

    if (isTRUE(perc_change)) {
        ref_level = lapply(output, function (x) {x = as.data.frame(x[,1]); colnames(x) = paste("ref_level", time_index[1]);x})
        output = lapply(output, function (x) {(x[,-1] - x[,-ncol(x)]) / x[,-ncol(x)]})
        output = lapply(1:length(output), function (x) {cbind(ref_level[[x]],output[[x]])}) # include the reference level in outputs
        names(output) = names(sim)
    }

    cat("100% \n")
    return (output)
}
