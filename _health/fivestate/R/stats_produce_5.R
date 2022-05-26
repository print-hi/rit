#' function to produce the mean and variance of a list of values
#'
#' @param input
#' the list of values to be studied
#' @return
#' mean and variance of the
#' @export
#'
#' @examples
#' stats_first_time_leave_H=stats_produce_5(first_time_stats(simulated_path,0))
stats_produce_5=function(input){
  output=matrix(nrow=1, ncol = 2)
  colnames(output) <- c('expected_value', 'st_dev')
  output[1]=mean(input, na.rm = TRUE)
  output[2]=sd(input, na.rm = TRUE)
  return(output)
}
