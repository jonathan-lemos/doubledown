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
		geom_linerange(aes(x = timestamp, ymin = low, ymax = high), data = data) +
		geom_rect(aes(xmin = timestamp - 0.4,
					  xmax = timestamp + 0.4,
					  ymin = pmin(open, close),
					  ymax = pmax(open, close)),
				  data = data,
				  fill = ifelse(data$close >= data$open, "darkgreen", "darkred"))
}
