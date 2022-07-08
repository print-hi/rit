esg_summary = function (paths = NULL, quantiles = seq(0, 1, 0.25)) {
    
    ##################
    # error messages #
    ##################
    if (!is.list(paths) | !is.data.frame(paths) | is.null(paths)) {
        stop ("Input must be a list or a dataframe. ")
    } else if (!min(quantiles) < 0 | !max(quantiles) > 1) {
        stope ("Quantiles must be between 0 and 1. ")
    }
    message("This function provides period-by-period summary statistics to time series data.")
    message("Caution when using this function: ")
    message("1. The rows must represent the time spots, while the columns must be the trajectories. ")
    message("2. Any NA, Inf will be ignored. ")
    
    ##################
    # initialisation #
    ##################
    
    stats = c(paste(quantiles*100, "%-tile", sep = ""), "Mean", "StdDev")
    
    
    lapply(paths, function (x) {
        x[,1:length(quantiles)] = t(apply(x, 1, quantile, probs = quantiles))
        x[,length(quantiles)+1] = t(apply(x, 1, mean))
        x[,1:length(quantiles)+1] = t(apply(x,1,sd))
    })
}