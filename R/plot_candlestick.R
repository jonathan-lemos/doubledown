#' Plot Candlestick
#'
#' Adds a candlestick chart to a ddplot.
#' The x-axis will be the timestamp of the data point.
#' The y-axis's "stick" will have "high" at the top and "low" at the bottom.
#' The y-axis's "candle" will be either green or red denoting a gain or loss respectively from "open" to "close".
#'
#' @param plot The ggplot.
#' @param data The dataset to use.
#' @return A candlestick component for a ddplot.
#' @export
plot_candlestick <- function() {
	list(
		 geom_linerange(aes(x = timestamp, ymin = low, ymax = high)),
		 geom_tile(aes(x = timestamp,
					   width = 0.8,
					   y = (open + close) / 2,
					   height = abs(open - close),
					   fill = ifelse(close >= open, "up", "down"))),
		 guides(fill = FALSE, colour = FALSE),
		 scale_fill_manual(values = c("up" = "darkgreen", "down" = "darkred"))
	)
}
