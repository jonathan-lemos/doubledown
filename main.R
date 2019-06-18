source(file = "import.R")

import("readr")
import("dplyr")
import("ggplot2")
import("hash")

source(file = "api_key.R")
source(file = "indicators.R")

get_csv <- function(symbol) {
	link <- paste("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=", symbol, "&datatype=csv&outputsize=full&apikey=", api_key, sep = "")
	response <- link %>% read.csv
	return(response[nrow(response) : 1,])
}

get_all <- function(symbols) {
	return(hash(symbols, symbols %>% lapply(get_csv)))
}

results <- c("TSLA", "AAPL") %>% get_all
# p1 <- ggplot() + geom_line(aes(x = as.numeric(row.names(res)), y = close), data = res)
# plot(p1)
