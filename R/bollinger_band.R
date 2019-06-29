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
		ma <- input[1]
		variance <- 0
		variance_queue <- queue()
		variance_queue %>% pushback(0)
		ret <- c(0)

		for (i in 2:length(input)) {
			ma <- ma + input[i]
			variance <- variance + (input[i] - ma / min(i, n)) ^ 2
			variance_queue %>% pushback(variance)
			if (i > n) {
				ma <- ma - input[i - n]
				variance <- variance - variance_queue %>% pop
			}
			ret[i] <- sqrt(variance / (min(i, n) - 1))
		}
		ret
	}

	mavg <- input %>% simple_moving_average(mavg_days)
	stddev <- input %>% bb_moving_sd(mavg_days) * n_stddev
	data.frame("bollinger_band_top" = mavg + stddev,
			   "bollinger_band_bottom" = mavg - stddev)
}
