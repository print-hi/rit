#' Apply Functions Over Each Matrix of 3D Array
#'
#' Returns a 3D array of matrices obtained by applying a function over each matrix
#' of a 3D array.
#'
#' @param X
#' 3D array
#' @param FUN
#' the function to be applied
#'
#' @return
#' 3D array of matrices obtained by applying a function
#'
#' @examples
#'
arr_apply <- function(X, FUN) {

  X_list <- lapply(seq(dim(X)[3]), function(i) matrix(X[, , i], nrow = dim(X)[1], ncol = dim(X)[2]))
  sapply(X_list, FUN, simplify = "array")
}
