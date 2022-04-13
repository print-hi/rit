#' get_afns_simulation
#' 
#' Returns the simulated paths of the zero-coupon interest rate term structure. The model is based on an Arbitrage-Free Nelson-Siegel (AFNS) model. This function uses the package `MASS` for random number generation and `expm` for matrix exponential. 
#'
#' @param num_years Number of years to forecast, counting from 2021-06-01. Default is 5 years. 
#' @param num_paths Number of simulation paths. Default is 10 paths. 
#' @param frequency One of "year", "quarter", and "month" (default).  
#' @param type Either "independent" (default) or "correlated". Independent-factor model assumes independence between the latent factors for interest rates. 
#' @param model Either "interest_rate" (default) or "interest_house_stock". The latter model is based on an augmented Arbitrage-Free Nelson-Siegel model, where both home value index and stock price are assumed to be dependent on interest rates. 
#'
#' @return If model is `interest_rate`, the function returns a list containing 40 data frames for the simulated trajectories for maturities from 1 quarter up to 10 years. If model is `interest_house_stock`,the function returns a list containing 42 data frames for the simulated trajectories for maturities from 1 quarter up to 10 years, as well as NSW house value indexes and S&P/ASX200 closing prices. 
#' @export
#'
#' @examples sim = get_afns_simulations(num_years = 10, num_paths = 100, frequency = "year", type = "correlated", model = "interest_rate"). To obtain trajectories of Australia 3-month zero-coupon yields, type sim$maturity_1qtrs. To obtain trajectories of S&P/ASX200 closing prices, type sim$stock_price. 
#' 
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
        
    } else {
        
        ##################
        # initialisation #
        ##################
        
        init_qtr = as.Date("2021-06-01")
        path_index = paste("trajectory_", 1:num_paths, sep = "")
        
        if (model == "interest_rate") {
            init_zcp = c(-0.0000666667,  0.0003857143,  0.0005095238,  0.0003571429,  0.0001000000, -0.0001000000, -0.0001571429, -0.0000476190,  0.0002238095,  0.0006333333,  0.0011523810,  0.0017523810,  0.0024047619,  0.0031000000, 0.0038142857,  0.0045238095,  0.0052428571,  0.0059571429,  0.0066380952,  0.0073000000,  0.0079523810, 0.0085809524,  0.0091952381,  0.0097761905,  0.0103476190,  0.0109047619,  0.0114190476,  0.0119380952, 0.0124333333,  0.0129095238,  0.0133714286,  0.0138142857,  0.0142476190,  0.0146666667,  0.0150857143, 0.0154714286,  0.0158523810,  0.0162238095,  0.0165952381,  0.0169333333)
            mat_qtrs = paste(paste("maturity",1:40,sep = "_"),"qtrs",sep = "")
            num_xt = 3
            num_zcp = 40
            mu0 = rep(0,num_xt)
            
            if (type == "independent") {
                
                ###########################################
                # independent-factor term-structure model #
                ###########################################
                
                ########################
                # estimated parameters #
                ########################
                
                if (frequency == "year") {
                    h = 1
                    Q_est = diag(c(0.0000503249, 0.0000615258, 0.000109023))
                    EK_est = diag(c(0.199374, 0.5687, 0.453543))
                } else if (frequency == "quarter") {
                    h = 1/4
                    Q_est = diag(c(0.0000147503, 0.0000259462, 0.0000405344))
                    EK_est = diag(c(0.054073, 0.189608, 0.140217))
                } else {
                    h = 1/12
                    Q_est = diag(c(5.10007e-06, 9.88515e-06, 0.0000148928))
                    EK_est = diag(c(0.018359, 0.06768, 0.049111))
                }

                init_xt = as.numeric(c(0.0431174, -0.0453477, -0.0422812))
                KP_est = diag(c(0.222361,0.840951,0.6043))
                theta_est = matrix(c(0.0404048, -0.0247579, 0.000506779), nrow = num_xt, ncol = 1, byrow = T)
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
                                    1, 0.297236, 0.2577355), nrow = num_zcp, byrow = T)
                A_fitted = c(1.89811e-06, 7.32411e-06, 1.59407e-05, 2.74808e-05, 4.17307e-05, 5.85172e-05, 7.76972e-05, 9.91507e-05, 1.22775e-04, 1.48478e-04, 1.76182e-04, 2.05811e-04, 2.37298e-04, 2.70581e-04,
                             3.05599e-04, 3.42296e-04, 3.80619e-04, 4.20516e-04, 4.61940e-04, 5.04845e-04, 5.49187e-04, 5.94924e-04, 6.42020e-04, 6.90436e-04, 7.40140e-04, 7.91101e-04, 8.43288e-04, 8.96676e-04,
                             9.51240e-04, 1.00696e-03, 1.06381e-03, 1.12177e-03, 1.18084e-03, 1.24099e-03, 1.30221e-03, 1.36450e-03, 1.42783e-03, 1.49221e-03, 1.55763e-03, 1.62408e-03)
                
            } else {

                ##########################################
                # correlated-factor term-structure model #
                ##########################################
                
                ########################
                # estimated parameters #
                ########################
                
                if (frequency == "year") {
                    h = 1
                    Q_est = matrix(c(0.000338524,  0.000157037, -0.000825218,
                                     0.000157037,  0.000276876, -0.000596317,
                                     -0.000825218, -0.000596317,  0.002810015), nrow = num_xt, byrow = T)
                    EK_est = matrix(c(0.374112, 0.121458, 0.077477,
                                      0.107225, 0.204036, 0.063376,
                                      0.098135, 0.034051, 0.112287), nrow = num_xt, byrow = T)
                } else if (frequency == "quarter") {
                    h = 1/4
                    Q_est = matrix(c(3.72760e-05, -8.02134e-06, -0.0001452404,
                                     -8.02134e-06,  3.81772e-05, -0.0000484377,
                                     -1.45240e-04, -4.84377e-05,  0.0010137486), nrow = num_xt, byrow = T)
                    EK_est = matrix(c(0.114466, 0.040144, 0.025441,
                                      0.036034, 0.057985, 0.019472,
                                      0.031293, 0.011600, 0.030805), nrow = num_xt, byrow = T)
                } else {
                    h = 1/12
                    Q_est = matrix(c(5.95587e-06, -6.84538e-06, -2.79466e-05,
                                     -6.84538e-06,  1.19354e-05,  8.06465e-06,
                                     -2.79466e-05,  8.06465e-06,  3.91469e-04), nrow = num_xt, byrow = T)
                    EK_est = matrix(c(0.040007, 0.014258, 0.009024,
                                      0.012841, 0.019928, 0.006809,
                                      0.011031, 0.004145, 0.010503), nrow = num_xt, byrow = T)
                }
                
                init_xt = c(0.197511, -0.201358, -0.147409)
                KP_est = matrix(c(0.491775, 0.1766396, 0.1117192,
                                  0.159350, 0.2428947, 0.0837042,
                                  0.136163, 0.0515049, 0.1275055), nrow = num_xt, byrow = T)
                theta_est = matrix(c(0.1338931, -0.0203194, -0.1900826), nrow = num_xt, ncol = 1, byrow = T)
                lambda_est = 0.0745159
                B_fitted = matrix(c(1, 0.990743, 0.00919962,
                                    1, 0.981600, 0.01817266,
                                    1, 0.972570, 0.02692385,
                                    1, 0.963650, 0.03545780,
                                    1, 0.954840, 0.04377902,
                                    1, 0.946138, 0.05189195,
                                    1, 0.937543, 0.05980093,
                                    1, 0.929052, 0.06751024,
                                    1, 0.920665, 0.07502404,
                                    1, 0.912379, 0.08234643,
                                    1, 0.904195, 0.08948142,
                                    1, 0.896110, 0.09643294,
                                    1, 0.888122, 0.10320486,
                                    1, 0.880232, 0.10980094,
                                    1, 0.872436, 0.11622490,
                                    1, 0.864734, 0.12248037,
                                    1, 0.857126, 0.12857091,
                                    1, 0.849608, 0.13450000,
                                    1, 0.842181, 0.14027106,
                                    1, 0.834842, 0.14588745,
                                    1, 0.827592, 0.15135245,
                                    1, 0.820427, 0.15666928,
                                    1, 0.813348, 0.16184110,
                                    1, 0.806354, 0.16687100,
                                    1, 0.799442, 0.17176201,
                                    1, 0.792612, 0.17651710,
                                    1, 0.785864, 0.18113919,
                                    1, 0.779194, 0.18563113,
                                    1, 0.772604, 0.18999571,
                                    1, 0.766091, 0.19423567,
                                    1, 0.759654, 0.19835371,
                                    1, 0.753294, 0.20235245,
                                    1, 0.747007, 0.20623447,
                                    1, 0.740794, 0.21000231,
                                    1, 0.734654, 0.21365844,
                                    1, 0.728585, 0.21720528,
                                    1, 0.722586, 0.22064522,
                                    1, 0.716658, 0.22398059,
                                    1, 0.710798, 0.22721367,
                                    1, 0.705005, 0.23034670), nrow = num_zcp, byrow = T)
                A_fitted = c(4.11471e-07, 1.73551e-06, 4.14291e-06, 7.84850e-06, 1.31059e-05, 2.02028e-05, 2.94563e-05, 4.12086e-05, 5.58237e-05, 7.36831e-05,
                             9.51830e-05, 1.20731e-04, 1.50743e-04, 1.85642e-04, 2.25854e-04, 2.71806e-04, 3.23925e-04, 3.82636e-04, 4.48361e-04, 5.21513e-04,
                             6.02503e-04, 6.91731e-04, 7.89588e-04, 8.96457e-04, 1.01271e-03, 1.13870e-03, 1.27478e-03, 1.42128e-03, 1.57853e-03, 1.74682e-03,
                             1.92646e-03, 2.11772e-03, 2.32087e-03, 2.53615e-03, 2.76380e-03, 3.00405e-03, 3.25708e-03, 3.52311e-03, 3.80229e-03, 4.09479e-03)
                
            }
        } else {
            
            init_zcp = c(-0.0000666667,  0.0003857143,  0.0005095238,  0.0003571429,  0.0001000000, -0.0001000000, -0.0001571429, -0.0000476190,  0.0002238095,  0.0006333333,  0.0011523810,  0.0017523810,  0.0024047619,  0.0031000000, 0.0038142857,  0.0045238095,  0.0052428571,  0.0059571429,  0.0066380952,  0.0073000000,  0.0079523810, 0.0085809524,  0.0091952381,  0.0097761905,  0.0103476190,  0.0109047619,  0.0114190476,  0.0119380952, 0.0124333333,  0.0129095238,  0.0133714286,  0.0138142857,  0.0142476190,  0.0146666667,  0.0150857143, 0.0154714286,  0.0158523810,  0.0162238095,  0.0165952381,  0.0169333333, 5.1364719541,  8.8631940360)
            mat_qtrs = c(paste(paste("maturity",1:40,sep = "_"),"qtrs",sep = ""), "house_index", "stock_price")
            num_xt = 5
            num_zcp = 42
            mu0 = rep(0,num_xt)
            
            if (type == "independent") {

                #####################################################################
                # independent-factor term-structure, house index, stock price model #
                #####################################################################

                ########################
                # estimated parameters #
                ########################

                if (frequency == "year") {
                    h = 1
                    Q_est = matrix(c(0.000216636, 0, 0, -0.000807984, -0.00117134,
                                     0, 0.000249628, 0,  0.001493568,  0.00256654,
                                     0, 0, 0.0280076,  0.153084849,  0.28616890,
                                     -0.000807984, 0.001493568, 0.1530848,  0.900788872,  1.67755195,
                                     -0.001171339, 0.002566538, 0.2861689,  1.677551951,  3.16089095), nrow = num_xt, byrow = T)
                    EK_est = matrix(c(0.951019, 0, 0,    0,    0,
                                      0, 0.935627, 0,    0,    0,
                                      0, 0, 0.666645,    0,    0,
                                      0.487635, 0.482381, 0,    1,    0,
                                      0.487635, 0.482381, 0,    0,    1), nrow = num_xt, byrow = T)
                } else if (frequency == "quarter") {
                    h = 1/4
                    Q_est = matrix(c(0.0000583047, 0, 0, -0.000230464, -0.000324752,
                                     0, 0.000068785, 0,  0.000368144,  0.000649956,
                                     0, 0, 0.0119581,  0.051434225,  0.096148480,
                                     -0.0002304637, 0.000368144, 0.0514342,  0.225073663,  0.419197096,
                                     -0.0003247524, 0.000649956, 0.0961485,  0.419197096,  0.789964510), nrow = num_xt, byrow = T)
                    EK_est = matrix(c(0.2467838, 0, 0, 0, 0,
                                      0, 0.2457390, 0, 0, 0,
                                      0, 0, 0.223983, 0, 0,
                                      0.0320186, 0.0319297, 0, 0.25, 0,
                                      0.0320186, 0.0319297, 0, 0, 0.25), nrow = num_xt, byrow = T)
                } else {
                    h = 1/12
                    Q_est = matrix(c(0.0000197612, 0, 0, -0.0000790914, -0.000110784,
                                     0, 0.0000234401, 0,  0.0001221608,  0.000217145,
                                     0, 0, 0.00457303,  0.0183956143,  0.034387810,
                                     -0.0000790914, 0.0001221608, 0.01839561,  0.0750169808,  0.139719572,
                                     -0.0001107844, 0.0002171447, 0.03438781,  0.1397195721,  0.263303490), nrow = num_xt, byrow = T)
                    EK_est = matrix(c(0.0829508, 0, 0, 0, 0,
                                      0, 0.08282562, 0, 0, 0,
                                      0, 0, 0.0801082, 0, 0,
                                      0.0038083, 0.00380463, 0, 1/12, 0,
                                      0.0038083, 0.00380463, 0, 0, 1/12), nrow = num_xt, byrow = T)
                }

                init_xt = c(1.224612, -1.226309,  0.721877,  5.136655,  8.863235)
                KP_est = matrix(c(0.100447,  0, 0, 0, 0,
                                  0, 0.133449, 0, 0, 0,
                                  0, 0, 0.865866, 0, 0, 
                                  -1, -1, 0, 0, 0,
                                  -1, -1, 0, 0, 0), nrow = num_xt, byrow = T)
                theta_est = matrix(c(0.0821409,-0.0778772,1.2816489,3.9180384,7.3372624), nrow = num_xt, ncol = 1, byrow = T)
                lambda_est = 0.00270891
                B_fitted = matrix(c(1, 0.999661, 0.000338461,    0,    0,
                                    1, 0.999323, 0.000676616,    0,    0,
                                    1, 0.998985, 0.001014467,    0,    0,
                                    1, 0.998647, 0.001352012,    0,    0,
                                    1, 0.998309, 0.001689252,    0,    0,
                                    1, 0.997971, 0.002026188,    0,    0,
                                    1, 0.997633, 0.002362819,    0,    0,
                                    1, 0.997296, 0.002699146,    0,    0,
                                    1, 0.996959, 0.003035169,    0,    0,
                                    1, 0.996621, 0.003370889,    0,    0,
                                    1, 0.996284, 0.003706305,    0,    0,
                                    1, 0.995948, 0.004041418,    0,    0,
                                    1, 0.995611, 0.004376228,    0,    0,
                                    1, 0.995274, 0.004710735,    0,    0,
                                    1, 0.994938, 0.005044940,    0,    0,
                                    1, 0.994602, 0.005378842,    0,    0,
                                    1, 0.994266, 0.005712443,    0,    0,
                                    1, 0.993930, 0.006045741,    0,    0,
                                    1, 0.993594, 0.006378738,    0,    0,
                                    1, 0.993258, 0.006711434,    0,    0,
                                    1, 0.992923, 0.007043828,    0,    0,
                                    1, 0.992587, 0.007375922,    0,    0,
                                    1, 0.992252, 0.007707715,    0,    0,
                                    1, 0.991917, 0.008039207,    0,    0,
                                    1, 0.991582, 0.008370400,    0,    0,
                                    1, 0.991247, 0.008701292,    0,    0,
                                    1, 0.990913, 0.009031884,    0,    0,
                                    1, 0.990578, 0.009362177,    0,    0,
                                    1, 0.990244, 0.009692171,    0,    0,
                                    1, 0.989910, 0.010021865,    0,    0,
                                    1, 0.989576, 0.010351261,    0,    0,
                                    1, 0.989242, 0.010680358,    0,    0,
                                    1, 0.988909, 0.011009157,    0,    0,
                                    1, 0.988575, 0.011337658,    0,    0,
                                    1, 0.988242, 0.011665860,    0,    0,
                                    1, 0.987908, 0.011993765,    0,    0,
                                    1, 0.987575, 0.012321373,    0,    0,
                                    1, 0.987242, 0.012648683,    0,    0,
                                    1, 0.986910, 0.012975696,    0,    0,
                                    1, 0.986577, 0.013302412,    0,    0,
                                    0, 0.000000, 0.000000000,    1,    0,
                                    0, 0.000000, 0.000000000,    0,    1), nrow = num_zcp, ncol = num_xt, byrow = T)
                A_fitted = c(5.45784e-06, 2.18241e-05, 4.90933e-05, 8.72581e-05, 1.36313e-04, 1.96253e-04, 2.67076e-04, 3.48779e-04, 4.41360e-04, 5.44819e-04, 6.59157e-04, 7.84374e-04, 9.20475e-04, 1.06746e-03, 1.22534e-03, 1.39411e-03, 1.57379e-03, 1.76438e-03,
                             1.96589e-03, 2.17832e-03, 2.40170e-03, 2.63602e-03, 2.88130e-03, 3.13757e-03, 3.40482e-03, 3.68307e-03, 3.97235e-03, 4.27266e-03, 4.58403e-03, 4.90648e-03, 5.24002e-03, 5.58467e-03, 5.94046e-03, 6.30741e-03, 6.68555e-03, 7.07489e-03,
                             7.47546e-03, 7.88729e-03, 8.31041e-03, 8.74484e-03, 0, 0)

            } else {

                ####################################################################
                # correlated-factor term-structure, house index, stock price model #
                ####################################################################

                ######################## UPATE REQUIRED! 
                # estimated parameters #
                ########################

            #     if (frequency == "year") {
            #         h = 1
            #         Q_est = matrix(c(,), nrow = num_xt, byrow = T)
            #         EK_est = matrix(c(,), nrow = num_xt, byrow = T)
            #     } else if (frequency == "quarter") {
            #         h = 1/4
            #         Q_est = matrix(c(,), nrow = num_xt, byrow = T)
            #         EK_est = matrix(c(,), nrow = num_xt, byrow = T)
            #     } else {
            #         h = 1/12
            #         Q_est = matrix(c(,), nrow = num_xt, byrow = T)
            #         EK_est = matrix(c(,), nrow = num_xt, byrow = T)
            #     }
            #   
            #     init_xt = c(, , )
            #     KP_est = matrix(c(), nrow = 5, byrow = T)
            #     theta_est = matrix(c(), nrow = 5, ncol = 1, byrow = T)
            #     lambda_est = 
            #     B_fitted = matrix(c(,), nrow = num_zcp, ncol = num_xt, byrow = T)
            #     A_fitted = c(,)
            }
        }

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
            zcp_sim = lapply(1:num_paths, function (x) {zcp_sim[[x]] = t(B_fitted %*% as.matrix(Xt_sim[[x]])) - A_fitted})
            zcp_sim = lapply(zcp_sim, function(x){ x = as.data.frame(x)[-1,]; row.names(x) = as.character(time_index); colnames(x) = mat_qtrs; return (x)})
            
            ###################
            # reorganise data # 
            ###################
            
            output = zcp_sim
            output = lapply(1:num_zcp, function (x) {lapply(1:num_paths, function (y) {zcp_sim[[y]][,x]})})
            output = lapply(output, function(x){x = as.data.frame(x); row.names(x) = as.character(time_index); colnames(x) = path_index; return (x)})
            names(output) = mat_qtrs
            if (model == "interest_house_stock") {
                output[[41]] = exp(output[[41]])
                output[[42]] = exp(output[[42]])
            }
        return (output)
    }
} 
