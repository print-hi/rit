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

    attr_mapping <- list(
        AP          = "Account Based Policy",
        CA          = "Care Annuity",
        LA          = "Life Annuity",
        PA          = "Pooled Annuity",
        VA          = "Variable Annuity",
        RM          = "Reverse Mortgage",
        bal         = "Balance     ",
        exp         = "Expense     ",
        benefit     = "Benefit     ",
        defer       = "Defer       ",
        increase    = "Increase    ",
        loading     = "Loading     ",
        size        = "Pool Size   ",
        state       = "States      ",
        min         = "Min Length  ",
        interest    = "Interest    ",
        value       = "Value       ",
        LVR         = "LVR         ",
        trans_cost  = "Trans. Cost ",
        length      = "Length      ",
        prop        = "Prop        ",
        g_fee       = "Guar. Fee   ",
        s_fee       = "Surr. Fee   "
    )


    msg <- c("========= Policy Details =========",
             paste("Type        :", attr_mapping[[policy$name[1]]]),
             "----------------------------------")

    for (i in colnames(policy)) {
        if (i != "name") {
            msg <- c(msg, paste(attr_mapping[[i]], ": ", policy[i], sep = ""))
        }
    }

    formatted <- function(x) {
        formatC(as.numeric(x), format="f", digits=2, big.mark=",")
    }

    msg <- c(msg, "",
             "======= Summary Statistics =======",
             paste("Mean        : $", formatted(stat$mean), sep = ""),
             paste("Std Dev     : $", formatted(stat$sd), sep = ""),
             "----------------------------------",
             paste("Minimum     : $", formatted(stat$min), sep = ""),
             paste("Maximum     : $", formatted(stat$max), sep = ""),
             "----------------------------------",
             paste("P_0.25      : $", formatted(stat$quantile[1]), sep = ""),
             paste("P_0.50      : $", formatted(stat$median), sep = ""),
             paste("P_0.75      : $", formatted(stat$quantile[3]), sep = ""),
             "----------------------------------",
             paste("Skewness    : ", formatted(stat$skew), sep = ""),
             paste("Kurtosis    : ", formatted(stat$kurtosis), sep = ""),
             "==================================")

    writeLines(msg)

    x <- list(paths = paths, stats = stat, conv = conv, dist = dist)

    ret <- structure(x, class = "cf")

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

    stats <- list(mean = mean(prices),
                  var = var(prices),
                  sd = sqrt(var(prices)),
                  min = min(prices),
                  max = max(prices),
                  skew = skewness(prices),
                  kurtosis = kurtosis(prices),
                  median = median(prices),
                  quantile = quantile(prices, probs = c(.25, .5, .75)))

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
