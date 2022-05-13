#' Combine Historical and Simulated Rates
#'
#' @param rates_hist
#' Matrix of historical mortality rates with age (on the rows) and
#' calendar year or cohort (on the columns)
#' @param rates_sim
#' 3D array of simulated mortality rates with age (on the rows) and
#' calendar year or cohort (on the columns) and simulation number (3rd dimension)
#'
#' @return
#' 3D array of combined historical and simulated rates
#' @export
#'
#' @examples
#'
combine_hist_sim <- function(rates_hist, rates_sim) {

# Flagging errors ---------------------------------------------------------

  if (!is.matrix(rates_hist)) {
    stop("historical rates must be a 2D matrix")
  }

  if (!(is.array(rates_sim) & length(dim(rates_sim)) == 3)) {
    stop("simulated rates must be a 3D array")
  }

  if (dim(rates_hist)[1] != dim(rates_sim)[1]) {
    stop("historical and simulated rates must have an equal number of rows")
  }


# Implementation ----------------------------------------------------------

  n_row = dim(rates_hist)[1]
  n_col = dim(rates_hist)[2] + dim(rates_sim)[2]
  n_sim = dim(rates_sim)[3]


  rates_all <- array(NA, dim = c(n_row, n_col, n_sim))
  for (i in 1:n_sim) {
    rates_all[,,i] = cbind(rates_hist, rates_sim[,,i])
  }

  rownames(rates_all) <- rownames(rates_hist)
  colnames(rates_all) <- c(colnames(rates_hist), colnames(rates_sim))

  rates_all
}


