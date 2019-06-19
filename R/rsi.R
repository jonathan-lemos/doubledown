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
	ret <- c()
	for (i in n:length(input)) {
		sample <- input[i - n:i] %>% pct_chg
		avg_gain <- sample[sample > 0] %>% mean
		avg_loss <- sample[sample < 0] %>% mean
		ret[i] = 100 - (100 / (1 + (avg_gain / avg_loss)))
	}
}
