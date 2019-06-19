#' Simple Moving Average
#'
#' Calculates a simple moving average of the input vector.
#' The output vector will be an arithmetic mean of the last \code{n} values.
#' The first \code{n} entries of the output vector will be a simple arithmetic mean of the values so far.
#'
#' @param input A vector of numerics
#' @param n The length of the moving average window.
#' @return A vector containing the simple moving average of the input.
#' @export
simple_moving_average <- function(input, n = 20) {
	acc <- 0
	ret <- c()
	for (i in 1:length(input)) {
		acc <- acc + input[i]
		if (i > n) {
			acc <- acc - input[i - n]
		}
		ret[i] <- acc / min(i, n)
	}
	ret
}
