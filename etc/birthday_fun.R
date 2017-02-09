rm(list = ls())
library(ggplot2)
library(gridExtra)

plot.fun <- function(message) {
  # generate draws from a normal distribution
	x <- seq(-2, 2, 0.01)
	y1 <- dnorm(x, 0, 1) 
	y2 <- dnorm(x, 0, 1) - .05
	y3 <- dnorm(x, 0, 1) - .1
	y4 <- dnorm(x, 0, 1) - .15
	y5 <- dnorm(x, 0, 1) - .2
	y6 <- dnorm(x, 0, 1) - .25

	# make data frames for plotting
	xddf1 <- data.frame(x = x, y = y1)
	xddf2 <- data.frame(x = x, y = y2)
	xddf3 <- data.frame(x = x, y = y3)
	xddf4 <- data.frame(x = x, y = y4)
	xddf5 <- data.frame(x = x, y = y5)
	xddf6 <- data.frame(x = x, y = y6)

	# plot distributions
	qplot(x, y, data = xddf1, geom = "line", colour=I("red"), ylim = c(0, .45), ylab = "", xlab = "") +
		geom_ribbon(data=xddf1, aes(ymax=y), ymin=0, fill="red", colour="red", alpha=0.6) +
		geom_ribbon(data=xddf2, aes(ymax=y), ymin=0, fill="orange", colour="orange", alpha=0.6) +
		geom_ribbon(data=xddf3, aes(ymax=y), ymin=0, fill="yellow", colour="yellow", alpha=0.6) +
		geom_ribbon(data=xddf4, aes(ymax=y), ymin=0, fill="green", colour="green", alpha=0.6) +
		geom_ribbon(data=xddf5, aes(ymax=y), ymin=0, fill="blue", colour="blue", alpha=0.6) +
		geom_ribbon(data=xddf6, aes(ymax=y), ymin=0, fill="violet", colour="violet", alpha=0.6) + 
		annotate("text", x = 0, y = 0.44, size = 7, label = message)
}

# automate this process for multiple birthdays
birthday.fun <- function(birthday.friend) {
	message <- paste0("Happy Birthday, ", birthday.friend, "!")
	plot.fun(message = message)
}

plot1 <- birthday.fun(birthday.friend = "FEDE")
plot2 <- birthday.fun(birthday.friend = "DAVID")

grid.arrange(plot1, plot2, ncol=2)