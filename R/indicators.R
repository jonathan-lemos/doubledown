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

