#' esg_summary
#' 
#' Provides period-by-period summary statistics including quantiles, mean, and standard deviation for time series data. 
#'
#'
#' @param paths List or dataframe of time series data. 
#' @param probs Numeric vector of probabilities with values in [0,1]. 
#' @param na.rm logical, if true, any NA and NaN's are removed from data before computing the statistics. 
#'
#' @return List of dataframe containing summary statistics for each period across trajectories. 
#' @export
#'
#' @examples sim = esg_afns_simulations(num_years = 10, num_paths = 100, frequency = "year", type = "correlated", model = "interest_rate"). 
#' This contains simulated paths from the continuous-time simulator. 
#' series_summ = esg_summary (paths = sim, probs = seq(0,1,0.2), na.rm = T)
esg_summary = function (paths, probs = seq(0, 1, 0.25), na.rm = TRUE) {
    
    ##################
    # error messages #
    ##################
    if (!is.list(paths) | !is.data.frame(paths) | is.null(paths)) {
        stop ("Paths must be a list or a dataframe. ")
    } else if (min(probs) < 0 | max(probs) > 1 | is.complex(probs)) {
        stop ("Probs must be between 0 and 1. ")
    }
    message("This function provides period-by-period summary statistics for time series data.")
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
            output[[x]] = t(rbind(apply(paths[[x]], 1, quantile, probs = probs, na.rm = na.rm),
                                  apply(paths[[x]], 1, mean, na.rm = na.rm),
                                  apply(paths[[x]], 1, sd, na.rm = na.rm)))
            }, simplify = F)
        output = lapply(output, function (x) {x = as.data.frame(x); colnames(x) = stats; return (x)})
        names(output) = names(paths)
        
    } else {
        output = data.frame(matrix(NA, nrow = nrow(paths), ncol = length(stats)))
        output = t(rbind(apply(paths[[x]], 1, quantile, probs = probs, na.rm = na.rm),
                         apply(paths[[x]], 1, mean, na.rm = na.rm),
                         apply(paths[[x]], 1, sd, na.rm = na.rm)))
        output = as.data.frame(output)
        colnames(output) = stats
    }
    
    
    return (output)
}
