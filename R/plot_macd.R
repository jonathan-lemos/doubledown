#' Plot MACD
#'
#' Adds the MACD to a ddplot.
#'
#' @param plot The ddplot.
#' @param fast The number of days in the fast exponential moving average.
#' @param slow The number of days in the slow exponential moving average.
#' @param signal The number of days in the signal line.
#' @return The same plot with the MACD added.
#' @export
plot_macd <- function(plot, fast = 12, slow = 26, signal = 9, color = "blue", signal_color = "orange") {
	tmp <- macd(plot$data$close, fast, slow, signal)

	plot %>%
		add_lines(y = tmp$macd, name = "MACD", yaxis = "y2") %>%
		add_lines(y = tmp$macd_signal, name = "MACD Signal", yaxis = "y2")
}
