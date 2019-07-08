#' Plot Bollinger Band
#'
#' Adds a bollinger band to a ddplot.
#' This should be added before other ddplot components to prevent the band from shadowing other components such as the candlestick chart.
#'
#' @param plot The ddplot.
#' @param mavg_days The number of days in the bollinger band moving average.
#' @param n_stddev The number of standard deviations the bollinger band encompasses.
#' @param color The color of the bollinger band.
#' @return A bollinger band component for a ddplot.
#' @export
plot_bollinger_band <- function(plot, mavg_days = 20, n_stddev = 2) {
	bb <- bollinger_band(plot$data$close, mavg_days, n_stddev)

	plot %>%
		add_lines(y = bb$bollinger_band_top,
				  line = list(color = "#0088AA"),
				  name = "Bollinger Band",
				  showlegend = FALSE,
				  hoverinfo = "none") %>%
	add_lines(y = bb$bollinger_band_bottom,
			  line = list(color = "#0088AA"),
			  fill = "tonexty",
			  fillcolor = "#00DDAA22",
			  name = "Bollinger Band",
			  showlegend = FALSE,
			  hoverinfo = "none")

}
