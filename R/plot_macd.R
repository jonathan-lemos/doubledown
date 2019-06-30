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

	(plot +
		geom_line(aes(y = tmp$macd), color = color) +
		geom_line(aes(y = tmp$macd_signal), color = signal_color)) %>%
		plot_scale_y(tmp$macd, tmp$macd_signal)
}
