#' Plot Bollinger Band
#'
#' Adds a bollinger band to a ddplot.
#' This should be added before other ddplot components to prevent the band from shadowing other components such as the candlestick chart.
#'
#' @param mavg_days The number of days in the bollinger band moving average.
#' @param n_stddev The number of standard deviations the bollinger band encompasses.
#' @param color The color of the bollinger band.
#' @return A bollinger band component for a ddplot.
#' @export
plot_bollinger_band <- function(mavg_days = 20, n_stddev = 2, color = "blue") {
	list(

		 geom_ribbon(aes(
						 ymin = bollinger_band(close, mavg_days, n_stddev)$bollinger_band_bottom,
						 ymax = bollinger_band(close, mavg_days, n_stddev)$bollinger_band_top,
						 ),
					 fill = color,
					 alpha = 0.1),
		 geom_line(aes(y = bollinger_band(close, mavg_days, n_stddev)$bollinger_band_bottom), colour = color),
		 geom_line(aes(y = bollinger_band(close, mavg_days, n_stddev)$bollinger_band_top), colour = color)
	)
}
