#' Double Down Plot
#'
#' Creates a ggplot() for use with doubledown's functions.
#'
#' @param data A data frame containing columns "close", "high", "low", "open", "timestamp", and "volume".
#'
dd_plot <- function(data, num_breaks = 5) {
	ggplot(data, aes(x = data$timestamp, close = close, high = high, low = low, open = open, volume = volume)) +
		scale_x_bd(business.dates = data$timestamp, labels = date_format("%b '%y"), max.major.breaks = num_breaks) +
		xlab("date") +
		ylab("close")
}
