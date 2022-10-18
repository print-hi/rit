#' esg_summary
#'
#' Provides period-by-period summary statistics including quantiles, mean, 
#' and standard deviation for time series data.
#'
#'
#' @param paths List or dataframe containing time series data.
#' @param probs Numeric vector of probabilities with values in [0,1]. 
#' Default is 0%, 25%, 50%, 75%, 100%-tiles.
#' @param na.rm logical, if true, any NA and NaN's are removed from data before 
#' computing the statistics. Default TRUE.
#'
#' @return List of dataframe containing summary statistics for each period 
#' across trajectories.
#' @export
#' 
#' @examples sim = esg_afns_simulator(num_years = 10, num_paths = 10, 
#' frequency = "year", type = "correlated", model = "interest_rate") 
#' series_summ = esg_summary (paths = sim, probs = seq(0,1,0.2), na.rm = TRUE)
esg_summary = function (paths, probs = seq(0, 1, 0.25), na.rm = TRUE) {

    ##################
    # error messages #
    ##################
    if (!is.list(paths) & !is.data.frame(paths) & is.null(paths)) {
        stop ("Paths must be a list or a dataframe. ")
    } else if (min(probs) < 0 | max(probs) > 1 | is.complex(probs)) {
        stop ("Probs must be between 0 and 1. ")
    }
    cat("This function provides period-by-period summary statistics for time series data.")
    cat("Caution when using this function: the columns must represent the time spots, while the rows must be the trajectories. ")

    ###############
    # calculation #
    ###############

    stats = c(paste(probs*100, "%-tile", sep = ""), "Mean", "StdDev")

    if (is.list(paths) & !is.data.frame(paths)) {
        output = replicate(n = length(paths),
                           expr = {data.frame(matrix(NA, ncol = nrow(paths[[1]]), nrow = length(stats)))},
                           simplify = F)

        output = sapply(1:length(paths), function (x) {
            output[[x]] = rbind(apply(paths[[x]], 2, stats::quantile, probs = probs, na.rm = na.rm),
                                  apply(paths[[x]], 2, mean, na.rm = na.rm),
                                  apply(paths[[x]], 2, stats::sd, na.rm = na.rm))
            }, simplify = F)
        output = lapply(output, function (x) {x = as.data.frame(x); row.names(x) = stats; return (x)})
        names(output) = names(paths)

    } else {
        output = data.frame(matrix(NA, ncol = nrow(paths), nrow = length(stats)))
        output = rbind(apply(paths, 2, stats::quantile, probs = probs, na.rm = na.rm),
                         apply(paths, 2, mean, na.rm = na.rm),
                         apply(paths, 2, stats::sd, na.rm = na.rm))
        output = as.data.frame(output)
        row.names(output) = stats
    }


    return (output)
}
