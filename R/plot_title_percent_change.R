#' Plot Title (percent change)
#'
#' Adds a title to a ddplot containing the percent change.
#' This will be of the format \code{title - ticker (+3.14%)} or \code{ticker (-0.58%)}
#'
#' @param plot The ddplot.
#' @param ticker The ticker the ddplot corresponds to.
#' @param title Optional. The title of the plot.
#' @return The same plot with
#' @export

plot_title_percent_change <- function(plot, ticker, title = "") {
	if (title != "") {
		title = paste(title, " - ", sep = "")
	}

	pchg <- paste(title, ticker, "(+0.00%)"
	if (nrow(data) > 1) {
		tmp <- (plot$data$close[nrow(plot$data)] / plot$data$close[nrow(plot$data) - 1] - 1) * 100
		tmps <- format(round(abs(tmp), 2), nsmall = 2)
		sgn <- "+"
		if (tmp < 0) {
			sgn <- "-"
		}
		pchg <- paste(title, ticker, "(", sgn, tmps, "%)", sep = "")
	}

	plot +
		ggtitle(pchg)
}
