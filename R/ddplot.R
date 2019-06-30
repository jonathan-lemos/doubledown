#' Double Down Plot
#'
#' Creates a ggplot() for use with doubledown's functions.
#'
#' @param data A data frame containing columns "close", "high", "low", "open", "timestamp", and "volume".
#' @param since The first date to display on the chart.
#' @param until The last date to display on the chart.
#' @param num_breaks The number of breaks on the x-axis.
#' @return A base plot for use with \code{plot_} functions.
#' @export
ddplot <- function(data, since = Sys.Date() - months(1), until = Sys.Date(), num_breaks = 5, ticker = NULL, title = "") {
	ret <- ggplot(data,
				  aes(x = data$timestamp,
					  close = close,
					  high = high,
					  low = low,
					  open = open,
					  volume = volume)) +
			scale_x_bd(business.dates = data$timestamp,
				   labels = date_format("%b '%y"),
				   max.major.breaks = num_breaks,
				   limits = c(since, until)) +
			xlab("date") +
			ylab("close")

	if (!is.null(ticker)) {
		if (title != "") {
			title = paste(title, " - ", sep = "")
		}

		pchg <- paste(title, ticker, " (+0.00%)")
		if (nrow(data) > 1) {
			tmp <- (data$close[nrow(data)] / data$close[nrow(data) - 1] - 1) * 100
			tmps <- format(round(abs(tmp), 2), nsmall = 2)
			sgn <- "+"
			if (tmp < 0) {
				sgn <- "-"
			}
			pchg <- paste(title, ticker, " (", sgn, tmps, "%)", sep = "")
		}
		print(str(ret))
		ret <- ret + ggtitle(pchg)
		print(str(ret))
	}

	else if (title != "") {
		ret <- ret + ggtitle(title)
	}

	print(str(ret))

	ret$since = since
	ret$until = until
	ret$gmin = NULL
	ret$gmax = NULL
	ret
}