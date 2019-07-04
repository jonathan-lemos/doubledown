#' Plot RSI
#'
#' Adds the RSI to a ddplot.
#'
#' @param plot The ddplot.
#' @param n The number of days in the RSI.
#' @param color The color of the drawn line.
#' @return The same plot with the RSI added.
#' @export
plot_rsi <- function(plot, n = 14, color = "red") {
	tmp <- rsi(plot$data$close, n)
	(plot +
	 	geom_line(aes(y = tmp, colour = color))) %>%
		plot_scale_y(tmp)
}
