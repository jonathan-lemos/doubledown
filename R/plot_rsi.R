#' Plot RSI
#'
#' Adds the RSI to a ddplot.
#'
#' @param plot The ddplot.
#' @param n The number of days in the RSI.
#' @param color The color of the drawn line.
#' @return The same plot with the RSI added.
#' @export
plot_rsi <- function(plot, n = 14, levels = c(30, 70), color = "red") {
	for (level in levels) {
		print(level)
		plot <- plot %>% plot_hline(level)
	}
	plot %>%
		add_lines(y = ~rsi(plot$data$close, n), yaxis = "y2")
}
