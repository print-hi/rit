###############################################################################
###### CF: Class Definition


#' Valuation of Policy
#'
#' Conducts valuation of a given policy, providing summary statistics and
#' convergence / distribution plots
#'
#' @param policy
#' Policy object containing necessary parameters (see create_policy_)
#' @param cashflows
#' (Optional) Matrix of cashflows (see simulate_cf), if 'cashflows' is not
#' provided, function will simulate cashflows for policy
#' @param seed
#' Seed choice for random sampling
#' @return
#' 'pol_value' object: contains summary summary statistics and
#' convergence + distribution plots
#' @export value_policy
#' @examples
#' value <- value_policy(policy_object)
value_policy <- function(policy, cashflows = NULL, seed = 9999) {

    # For complex policies, would be faster to simulate_cf once
    # and pass in as a parameter
    if (is.null(cashflows)) cashflows <- simulate_cf(policy)

    if (!is.matrix(cashflows)) stop("Invalid Cashflow object")

    # Calculate price of policy for each path
    paths <- get_path_prices(cashflows)

    # Produce statistics and plots for policy price
    stat <- get_price_stats(paths)
    dist <- plot_convergence(paths, seed)
    conv <- plot_distribution(paths)

    # Maps 'colname' attribute to formatted title for output text
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
        s_fee       = "Surr. Fee   ",
        margin      = "Margin      "
    )

    # Format introduction for output text
    msg <- c("========= Policy Details =========",
             paste("Type        :", attr_mapping[[policy$name[1]]]),
             "----------------------------------")

    # Format attribute elements of policy into output text
    for (i in colnames(policy)) {
        if (i != "name") {
            msg <- c(msg, paste(attr_mapping[[i]], ": ", policy[i], sep = ""))
        }
    }

    # Function for consistent formatting for numeric (financial) variables
    formatted <- function(x) {
        formatC(as.numeric(x), format="f", digits=2, big.mark=",")
    }

    # Format summary statistics
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
             paste("P_0.95      : $", formatted(stat$quantile[4]), sep = ""),
             paste("P_0.99      : $", formatted(stat$quantile[5]), sep = ""),
             "----------------------------------",
             paste("Skewness    : ", formatted(stat$skew), sep = ""),
             paste("Kurtosis    : ", formatted(stat$kurtosis), sep = ""),
             "==================================")

    # Print output text
    writeLines(msg)

    # Create policy class object
    x <- list(paths = paths, stats = stat, conv = conv, dist = dist)
    ret <- structure(x, class = "pol_value")

    return(ret)
}

###############################################################################
###### Economic Scenario Generator Module

# Temporary helper function, should link to economic module
get_sdf <- function(n = 1000, period = 100) {
    interest <- as.matrix(read.csv("R/data/sdf.csv", header = FALSE))
    colnames(interest) <- NULL
    rownames(interest) <- NULL
    return(interest)
}

###############################################################################
###### HELPER FUNCTIONS

#' Valuation of Cashflows
#'
#' Calculates value of a policy using a provided set of cashflows
#'
#' @name get_path_prices
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @return
#' Policy price
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

#' Calculates statistics for Cashflows
#'
#' Calculates general statistics for a provided set of cashflows
#'
#' @name get_price_stats
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @return
#' List of statistics
get_price_stats <- function(prices) {

    # Percentiles to calculate
    probs = c(.25, .5, .75, .95, .99)

    # Calculate and organise summary statistics into list
    stats <- list(mean = mean(prices),
                  var = var(prices),
                  sd = sqrt(var(prices)),
                  min = min(prices),
                  max = max(prices),
                  skew = skewness(prices),
                  kurtosis = kurtosis(prices),
                  median = median(prices),
                  quantile = quantile(prices, probs = probs))

    # Otherwise, return expected value
    return(stats)

}

#' Plots Convergence of Cashflows
#'
#' Plots a convergence value for a provided set of cashflows
#'
#' @name plot_convergence
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param seed
#' Seed choice for random sampling
#' @return
#' Convergence Plot
plot_convergence <- function(prices, seed = 9999) {

    # Create break point for 100 points
    breaks <- seq((length(prices))/100, length(prices), (length(prices))/100)

    # If less than 100 points, break point for each price
    if (length(prices) < 100) breaks <- seq(1, length(prices))

    # Set seed value for random sampling
    set.seed(seed)

    # Record cumulative mean up until each break point
    expected <- rep(0, length(breaks))
    for (i in seq(1, length(breaks))) {
        expected[i] <- mean(sample(prices, size = breaks[i]), replace = F)
    }

    # Format plot
    title <- paste("Convergence of Policy Valuation (", length(prices),
                   " paths)", sep = "")
    plot(x = breaks, y = expected, ylab = "Value", xlab = "Number of Paths",
         main = title)
    abline(h = expected[length(expected)], lty=2)
    p <- recordPlot()

    return(p)

}

#' Plot Distribution of Cashflows
#'
#' Plots a histogram for a provided set of cashflows
#'
#' @name plot_distribution
#' @param cashflows
#' Matrix of simulated cashflow paths
#' @param convergence
#' If convergence = TRUE, plots convergence of price
#' @return
#' Distribution Plot
plot_distribution <- function(prices) {

    # Format histogram plot
    title <- paste("Distribution of Policy Valuation (", length(prices),
                   " paths)", sep = "")
    hist(x = prices, breaks = 20, ylab = "Frequency", xlab = "Value",
         main = title, labels = TRUE)

    p <- recordPlot()

    return(p)

}
