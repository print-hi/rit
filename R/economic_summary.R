esg_summary = function (paths = NULL, probs = seq(0, 1, 0.25)) {
    
    ##################
    # error messages #
    ##################
    if (!is.list(paths) | !is.data.frame(paths) | is.null(paths)) {
        stop ("Input must be a list or a dataframe. ")
    } else if (!min(probs) < 0 | !max(probs) > 1 | is.complex(probs)) {
        stope ("Quantiles must be between 0 and 1. ")
    }
    message("This function provides period-by-period summary statistics to time series data.")
    message("Caution when using this function: ")
    message("1. The rows must represent the time spots, while the columns must be the trajectories. ")
    message("2. Any NAs will be ignored. ")
    
    ###############
    # calculation #
    ###############
    
    stats = c(paste(probs*100, "%-tile", sep = ""), "Mean", "StdDev")
    
    if (is.list(paths)) {
        output = replicate(n = length(paths),
                           expr = {data.frame(matrix(NA, nrow = nrow(paths[[1]]), ncol = length(stats)))},
                           simplify = F)
        
        output = sapply(1:length(paths), function (x) {
            output[[x]] = t(rbind(apply(paths[[x]], 1, quantile, probs = probs, na.rm = TRUE),
                                  apply(paths[[x]], 1, mean, na.rm = TRUE),
                                  apply(paths[[x]], 1, sd, na.rm = TRUE)))
            }, simplify = F)
        output = lapply(output, function (x) {x = as.data.frame(x); colnames(x) = stats; return (x)})
        names(output) = names(paths)
        
    } else {
        output = data.frame(matrix(NA, nrow = nrow(paths), ncol = length(stats)))
        output = t(rbind(apply(paths[[x]], 1, quantile, probs = probs, na.rm = TRUE),
                         apply(paths[[x]], 1, mean, na.rm = TRUE),
                         apply(paths[[x]], 1, sd, na.rm = TRUE)))
        output = as.data.frame(output)
        colnames(output) = stats
    }
    
    
    return (output)
}
