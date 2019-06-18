source(file = "import.R")
import("magrittr")
import("dequer")

pct_chg <- function(input) {
	ret <- c(NA)
	for (i in 2:length(input)) {
		ret[i] = (input[i] / input[i - 1]) * 100
	}
	ret
}

turnaround <- function(input) {
	sign(input - lag(input)) != sign(lag(input) - lag(input, 2))
}

cross <- function(input1, input2) {
	tmp <- input1 - input2
	sign(tmp) != sign(lag(tmp))
}

sma <- function(input, n = 20) {
	acc <- 0
	ret <- c()
	for (i in 1:length(input)) {
		acc <- acc + input[i]
		if (i > n) {
			acc <- acc - input[i - n]
		}
		ret[i] <- acc / min(i, n)
	}
	ret
}

ema <- function(input, n = 20) {
	multiplier <- 2 / (n + 1)
	acc <- input[1]
	ret <- c(input[1])
	for (i in 2:length(input)) {
		acc <- (acc * (1 - multiplier)) + (input[i] * multiplier)
		ret[i] <- acc
	}
	ret
}

macd <- function(input, fast = 12, slow = 26, signal = 9) {
	tmp <- input %>% ema(fast) - input %>% ema(slow)
	data.frame("macd" = tmp,
			   "macd_signal" = tmp %>% ema(signal))
}

rsi <- function(input, n = 14) {
	sample <- input[1:n] %>% pct_chg
	avg_gain <- sample[sample > 0] %>% mean
	avg_loss <- sample[sample < 0] %>% mean
	100 - (100 / (1 + (avg_gain / avg_loss)))
}

bb_moving_sd <- function(input, n = 20) {
	mavg <- input %>% sma(n)
	ma <- input[1]
	variance <- 0
	variance_queue <- queue()
	variance_queue %>% pushback(0)
	ret <- c(NA)

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

bb <- function(input, mavg_days = 20, n_stddev = 2) {
	mavg <- input %>% sma(mavg_days)
	stddev <- bb_moving_sd(mavg_days) * n_stddev
	data.frame("bb_top" = mavg + stddev,
			   "bb_bot" = mavg - stddev)
}

bb_div <- function(input, mavg_days = 20, n_stddev = 2) {
	input %>% bb_moving_sd(mavg_days) * n_stddev
}

fib_retrace <- function(input, level, n_days = 30) {
	selection <- input %>% tail(n_days)
	minimum <- selection %>% min
	maximum <- selection %>% max
	fib_levels <- c(0, 0.236068, 0.381966, 0.5, 0.618034, 1)
	row_names <- paste("fib", 1:length(c), sep = "")
	calcs <- minimum + ((maximum - minimum) * fib_levels)
	data.frame(calcs, row.names = row_names)
}

