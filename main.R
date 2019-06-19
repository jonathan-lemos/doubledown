source(file = "import.R")

import("readr")
import("dplyr")
import("ggplot2")
import("hash")

source(file = "api_key.R")
source(file = "indicators.R")

get_csv <- function(symbol) {
	link <- paste("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=", symbol, "&datatype=csv&outputsize=full&apikey=", api_key, sep = "")
	response <- link %>% read.csv %>% mutate(timestamp = as.Date(timestamp))
	return(response[nrow(response) : 1,])
}

get_all <- function(symbols) {
	return(hash(symbols, symbols %>% lapply(get_csv)))
}

format_date <- function(plot, data) {
	plot +
		scale_x_date(breaks = data$timestamp) +
		theme(axis.text.x = element_text(angle = 45))
}

add_candlestick <- function(plot, data) {
	plot +
		geom_linerange(aes(x = timestamp, ymin = low, ymax = high)) +
		geom_rect(aes(xmin = timestamp - 0.4,
					  xmax = timestamp + 0.4,
					  ymin = pmin(open, close),
					  ymax = pmax(open, close)),
				  fill = ifelse(data$close >= data$open, "darkgreen", "darkred"))
}

results <- c("TSLA", "AAPL") %>% get_all
p1 <- results[["TSLA"]] %>% tail(30) %>%
	ggplot %>%
	add_candlestick(results[["TSLA"]] %>% tail(30))
p1 %>% plot
# p1 <- ggplot() + geom_line(aes(x = as.numeric(row.names(res)), y = close), data = res)
# plot(p1)
