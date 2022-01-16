# ------------------------------------------------------------------------
# ---- Economic Scenario Generator Module

# Temporary helper function, should link to economic module
get_sdf <- function(n = 1000, period = 100) {
    interest <- as.matrix(read.csv("R/data/sdf.csv", header = FALSE))
    colnames(interest) <- NULL
    rownames(interest) <- NULL
    return(interest)
}

# ------------------------------------------------------------------------
# ---- CF: Class Definition

#' Title
#'
#' @param cashflows
#'
#' @return
#' @export value_cf
#'
#' @examples
value_cf <- function(policy, cashflows) {

    paths <- get_path_prices(cashflows)

    stat <- get_price_stats(paths)
    dist <- plot_convergence(paths)
    conv <- plot_distribution(paths)

    x <- list(paths = paths, stats = stat, conv = conv, dist = dist)

    ret <- structure(x, class = "cf")

    msg <- c(paste("Valuation of", policy$name), "",
             "----- Policy Details -----")

    for (i in colnames(policy)) {
        if (i != "name") msg <- c(msg, paste(i, ": ", policy[i], sep = ""))
    }

    mean = round(stat$mean, 2)
    var = round(stat$var, 2)
    sd = round(sqrt(stat$var), 2)


    msg <- c(msg,
             "--------------------------\n",
             "--- Summary Statistics ---",
             paste("Mean       : ", mean, sep = ""),
             paste("StDev      : ", sd, sep = ""),
             paste("Variance   : ", var, sep = ""),
             "--------------------------")

    writeLines(msg)

    return(ret)
}


#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name get_path_prices
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param ret_paths
#' If ret_paths = TRUE, vector of prices for each path is returned
#' @return
#' Value of policy
#' @export get_path_prices
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
get_path_prices <- function(cashflows) {

    # For debugging purposes
    n_paths <- 10000
    periods <- 100

    # Extract matrix dimensions
    n_paths <- nrow(cashflows)
    periods <- ncol(cashflows)

    # Get Stochastic Discount Factors from ESG module and calculate
    # cumulative product of factors
    sdf <- get_sdf(n_paths, periods)
    cmsdf <- rowCumprods((1/sdf))

    # Calculate discounted value of cashflows for each path
    value <- rowSums(cashflows * cmsdf)

    return(value)


}

#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name get_price_stats
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param ret_paths
#' If ret_paths = TRUE, vector of prices for each path is returned
#' @return
#' Value of policy
#' @export get_price_stats
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
get_price_stats <- function(prices) {

    stats <- list(mean = mean(prices), var = var(prices))

    # Otherwise, return expected value
    return(stats)

}


#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name plot_convergence
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param convergence
#' If convergence = TRUE, plots convergence of price
#' @return
#' Value of policy
#' @export plot_convergence
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
plot_convergence <- function(prices) {

    breaks <- seq((length(prices))/100, length(prices), (length(prices))/100)
    expected <- rep(0, length(breaks))

    for (i in seq(1, length(breaks))) expected[i] <- mean(prices[1:breaks[i]])

    title <- paste("Convergence of Policy Valuation (", length(prices),
                   " paths)", sep = "")

    plot(x = breaks, y = expected, ylab = "Value", xlab = "Number of Paths",
         main = title)

    p <- recordPlot()

    return(p)

}

#' Valuation via cashflows
#'
#' Simulate cash flows using Monte-Carlo methods for various policies
#' @name plot_distribution
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param convergence
#' If convergence = TRUE, plots convergence of price
#' @return
#' Value of policy
#' @export plot_distribution
#' @examples
#' cf <- cashflow(policy = "VA", age = 65, sex = "M", n = 1000)
plot_distribution <- function(prices) {

    title <- paste("Distribution of Policy Valuation (", length(prices),
                   " paths)", sep = "")

    hist(x = prices, breaks = 20, ylab = "Frequency", xlab = "Value",
         main = title)

    p <- recordPlot()

    return(p)

}
