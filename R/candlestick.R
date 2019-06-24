#' Candlestick
#'
#' Adds a candlestick chart to a ggplot.
#' The x-axis will be the timestamp of the data point.
#' The y-axis's "stick" will have "high" at the top and "low" at the bottom.
#' The y-axis's "candle" will be either green or red denoting a gain or loss respectively from "open" to "close".
#'
#' @param plot The ggplot.
#' @param data The dataset to use.
#' @return The same ggplot with the candlestick chart added.
#' @export
candlestick <- function(plot, data) {
	plot +
#		geom_boxplot(aes(x = data$timestamp, ymin = data$low, ymax = data$high, lower = pmin(data$open, data$close), upper = pmax(data$open, data$close), middle = rep(NA, nrow(data)), stat = "identity")

		geom_linerange(aes(x = timestamp, ymin = low, ymax = high), data = data) +
		geom_tile(aes(x = timestamp,
					  width = 0.8,
					  y = (open + close) / 2,
					  height = abs(open - close)),
				  data = data,
				  fill = ifelse(data$close >= data$open, "darkgreen", "darkred"))
}
