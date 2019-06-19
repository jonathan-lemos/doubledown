#' Exponential Moving Average
#'
#' Calculates an exponential moving average of the input vector.
#' Each value in the output vector equals \code{200 / (n + 1)} percent the corresponding input value, with the remainder being the previous value.
#'
#' @param input A vector of numerics
#' @param n The length of the moving average window.
#' @return A vector containing the exponential moving average of the input.
#' @export
exponential_moving_average <- function(input, n = 20) {
	multiplier <- 2 / (n + 1)
	acc <- input[1]
	ret <- c(input[1])
	for (i in 2:length(input)) {
		acc <- (acc * (1 - multiplier)) + (input[i] * multiplier)
		ret[i] <- acc
	}
	ret
}
