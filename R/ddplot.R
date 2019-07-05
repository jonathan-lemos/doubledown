#' Double Down Plot
#'
#' Creates a ggplot() for use with doubledown's functions.
#'
#' @param data A data frame containing columns "close", "high", "low", "open", "timestamp", and "volume".
#' @param since The first date to display on the chart. If this is NULL, it will display all values on the chart until \code{until}.
#' @param until The last date to display on the chart. If this is NULL, it will display all values since \code{since}.
#' @return A base plot for use with \code{plot_} functions.
#' @export
ddplot <- function(data, since = NULL, until = NULL, ticker = NULL, title = "") {
	if (is.null(since)) {
		since = as.Date("1000-01-01")
	}
	if (is.null(until)) {
		until = Sys.Date()
	}

	# build the chart's title based on ticker and title
	pchg <- title
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
	}

	ret <- plot_ly(
				   x = data$timestamp
				   ) %>%
	layout(
		   title = pchg,
		   xaxis = list(
						autorange = TRUE,
						fixedrange = FALSE,
						type = "category",
						title = "Date"
						),
		   yaxis = list(
						autorange = TRUE,
						fixedrange = FALSE,
						title = "Price"
		   )
	)

	ret$data = data
	ret
}
