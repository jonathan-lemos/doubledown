import <- function(name) {
	if (!require(name, character.only = TRUE)) {
		install.packages(name)
		library(name, character.only = TRUE)
	}
}

import("readr")
import("dplyr")
import("ggplot2")

source(file = "api_key.R")

get_csv <- function(symbol) {
	link <- paste("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=", symbol, "&datatype=csv&apikey=", api_key, sep = "")
	link %>% read.csv %>% return
}

get_csv("TSLA") %>% head %>% print
