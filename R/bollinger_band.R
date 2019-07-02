#' Bollinger Band
#'
#' Computes the "bollinger band", which is a 20-day SMA +/- 2 standard deviations of an input vector.
#'
#' @param input The input vector.
#' @param mavg_days The amount of days in the simple moving average.
#' @param n_stddev The amount of standard deviations from the moving average the band should span.
#' @return A data frame containing rows \code{"bollinger_band_top"} and \code{"bollinger_band_bottom"} corresponding to the top and bottom of the band.
#' @export
bollinger_band <- function(input, mavg_days = 20, n_stddev = 2) {
	bb_moving_sd <- function(input, n = 20) {
		# The first value should be 0 because one value has no deviation.
		# The remaining values are a standard deviation of the previous 20 values, or all previous values if there are less than 20.
		c(0, 2:length(input) %>% sapply(function(i) input[max(1, i - n + 1) : i] %>% sd ))
	}

	mavg <- input %>% simple_moving_average(mavg_days)
	stddev <- input %>% bb_moving_sd(mavg_days) * n_stddev
	data.frame("bollinger_band_top" = mavg + stddev,
			   "bollinger_band_bottom" = mavg - stddev)
}
