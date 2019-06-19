#' Moving Average Convergence Divergence
#'
#' The MACD is determined by subtracting a 26-day EMA from a 12-day EMA. A 9-day EMA of the MACD, called the "signal line", is then plotted alongside it.
#'
#' @param input A vector of numerics
#' @param fast The period of the "fast" EMA. By default this is 12.
#' @param slow The period of the "slow" EMA. By default this is 26.
#' @param signal The period of the "signal" EMA. By default this is 9.
#' @return A data frame containing columns "macd" and "macd_signal", corresponding to two rows containing the MACD and signal line.
#' @export
macd <- function(input, fast = 12, slow = 26, signal = 9) {
	tmp <- input %>% ema(fast) - input %>% ema(slow)
	data.frame("macd" = tmp,
			   "macd_signal" = tmp %>% ema(signal))
}
