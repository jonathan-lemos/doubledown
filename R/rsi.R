#' Relative Strength Index
#'
#' Calculates the "relative strength" of the input vector over a period of time.
#' This is equal to \code{100 - (100 / (1 + (average_gain / average_loss)))}, with \code{average_gain} being the average increase in a value compared to the previous day, and vice versa. The first n values of the output vector will be NA.
#'
#' @param input A vector of numerics
#' @param n The number of values to compute the RSI over. By default this is 14.
#' @return A vector containing the RSI of the input.
#' @export
rsi <- function(input, n = 14) {
	function rs(gain, loss) 100 - (100 / (1 + (gain / loss)))

	init_sample <- input[2:(n + 1)] - input[1:n]
	avg_gain <- init_sample[init_sample > 0] %>% mean
	avg_loss <- init_sample[init_sample < 0] %>% mean
	ret <- c()
	ret[n + 1] = rs(avg_gain, avg_loss)

	for (i in (n + 2):length(input)) {
		diff <- input[i] - input[i - 1]
		if (diff > 0) {
			avg_gain <- (avg_gain * (n - 1) + diff) / n
			avg_loss <- avg_loss * (n - 1) / n
		}
		else {
			avg_gain <- avg_gain * (n - 1) / n
			avg_loss <- (avg_loss * (n - 1) + diff) / n
		}
		ret[i] = rs(avg_gain, avg_loss)
	}
	ret
}
