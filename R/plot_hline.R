#' Plot Horizontal Line
#'
#' Adds the RSI to a ddplot.
#'
#' @param plot The ddplot.
#' @param y Where to draw the line.
#' @param color The color of the drawn line.
#' @return The same plot with the RSI added.
#' @export
plot_hline <- function(plot, y = 0) {
	tmp <- y # needed otherwise the formula puts multiple lines on the same y level
	plot %>%
		add_lines(y = ~tmp, yaxis = "y2", hoverinfo = "none")
}
