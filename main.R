if (!require(readr)) {
	install.packages("readr")
	library(readr)
}

temp <- read.csv("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=MSFT&apikey=demo&datatype=csv")
print(temp)
