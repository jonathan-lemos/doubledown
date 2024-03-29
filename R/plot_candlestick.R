#' Plot Candlestick
#'
#' Adds a candlestick chart to a ddplot.
#' The x-axis will be the timestamp of the data point.
#' The y-axis's "stick" will have "high" at the top and "low" at the bottom.
#' The y-axis's "candle" will be either green or red denoting a gain or loss respectively from "open" to "close".
#'
#' @param plot The ddplot.
#' @return The plot with a candlestick component added.
#' @export
plot_candlestick <- function(plot) {
	plot %>%
		add_trace(
				  type = "candlestick",
				  open = ~open,
				  close = ~close,
				  low = ~low,
				  high = ~high,
				  yaxis = "y2",
				  name = "Price"
		)
}
