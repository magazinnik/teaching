rm(list = ls())

library("ggplot2")
library("MASS")
library("scales")

load("data/regs.RData")

## This will load output from the following regressions: 
#exp <- lm(Y ~ ed.rev.state.pc.0709_log + ed.rev.state.pc_log + wint + loset + winrev0709 + loserev0709 +
#				 		  gov_dem + prop_hou_dem + vs_dem_08 + control_2008 + last_f + factor(appsection) - 1,
#				 		  data=sub.exp)
#
#cheap <- lm(Y ~ ed.rev.state.pc.0709_log + ed.rev.state.pc_log + wint + loset + winrev0709 + loserev0709 +
#						    gov_dem + prop_hou_dem + vs_dem_08 + control_2008 + last_f + factor(appsection) - 1,
#					      data=sub.cheap)

## compute predicted probabilities

# get means of model matrix
means.exp <- apply(model.matrix(exp), 2, mean, na.rm=T)
means.cheap <- apply(model.matrix(cheap), 2, mean, na.rm=T)

# expand to matrix 
points <- seq(from=-.26, to=.23, by=.001)

mat.exp <- matrix(rep(means.exp, each=length(points)), nrow=length(points))
mat.cheap <- matrix(rep(means.cheap, each=length(points)), nrow=length(points))

colnames(mat.exp) <- names(means.exp)
colnames(mat.cheap) <- names(means.cheap)

mat.exp[,"ed.rev.state.pc.0709_log"] <- points
mat.cheap[,"ed.rev.state.pc.0709_log"] <- points

# for expensive policies, winners and losers
mat.exp.win <- mat.exp.lose <- mat.exp
mat.exp.win[,"wint"] <- 1
mat.exp.win[,"loset"] <- 0 
mat.exp.win[,"winrev0709"] <- mat.exp.win[,"wint"] * mat.exp.win[,"ed.rev.state.pc.0709_log"]
mat.exp.win[,"loserev0709"] <- mat.exp.win[,"loset"] * mat.exp.win[,"ed.rev.state.pc.0709_log"]

mat.exp.lose[,"wint"] <- 0
mat.exp.lose[,"loset"] <- 1 
mat.exp.lose[,"winrev0709"] <- mat.exp.lose[,"wint"] * mat.exp.lose[,"ed.rev.state.pc.0709_log"]
mat.exp.lose[,"loserev0709"] <- mat.exp.lose[,"loset"] * mat.exp.lose[,"ed.rev.state.pc.0709_log"]

# for cheap policies, winners and losers
mat.cheap.win <- mat.cheap.lose <- mat.cheap
mat.cheap.win[,"wint"] <- 1
mat.cheap.win[,"loset"] <- 0 
mat.cheap.win[,"winrev0709"] <- mat.cheap.win[,"wint"] * mat.cheap.win[,"ed.rev.state.pc.0709_log"]
mat.cheap.win[,"loserev0709"] <- mat.cheap.win[,"loset"] * mat.cheap.win[,"ed.rev.state.pc.0709_log"]

mat.cheap.lose[,"wint"] <- 0
mat.cheap.lose[,"loset"] <- 1 
mat.cheap.lose[,"winrev0709"] <- mat.cheap.lose[,"wint"] * mat.cheap.lose[,"ed.rev.state.pc.0709_log"]
mat.cheap.lose[,"loserev0709"] <- mat.cheap.lose[,"loset"] * mat.cheap.lose[,"ed.rev.state.pc.0709_log"]

## get predicted probabilities
y.exp.win <- data.frame(points, mat.exp.win %*% coef(exp))
y.exp.lose <- data.frame(points, mat.exp.lose %*% coef(exp))
y.cheap.win <- data.frame(points, mat.cheap.win %*% coef(cheap))
y.cheap.lose <- data.frame(points, mat.cheap.lose %*% coef(cheap))

names(y.exp.win) <- names(y.exp.lose) <- names(y.cheap.win) <- names(y.cheap.lose) <-
	c("points", "predval")

## compute standard errors by quasi-bayesian simulation 
set.seed(5)

CI.mc <- function(model, mat) {
	# simulate beta matrix
	beta.sim <- mvrnorm(n = 100000, mu = coef(model), Sigma = vcov(model))
	lower <- upper <- rep(NA, nrow(mat))
	for (i in 1:nrow(mat)) {
		y.hat <- beta.sim %*% mat[i,]
		lower[i] <- qnorm(.025, mean=mean(y.hat), sd=sd(y.hat)) 
		upper[i] <- qnorm(.975, mean=mean(y.hat), sd=sd(y.hat)) 
	}
	CI <- data.frame(lower, upper)
	return(CI)
}

CI.exp.win <- CI.mc(model = exp, mat = mat.exp.win)
CI.exp.lose <- CI.mc(model = exp, mat = mat.exp.lose)
CI.cheap.win <- CI.mc(model = cheap, mat = mat.cheap.win)
CI.cheap.lose <- CI.mc(model = cheap, mat = mat.cheap.lose)

## make plots
p.exp <- ggplot(data = y.exp.win, aes(points)) 
ggsave(p.exp + geom_ribbon(aes(ymin = CI.exp.win$lower, ymax = CI.exp.win$upper), fill = "grey70", alpha = 0.3) +
 	   geom_line(aes(y = y.exp.win$predval), col = "red") + 
 	   geom_ribbon(aes(ymin = CI.exp.lose$lower, ymax = CI.exp.lose$upper), fill = "grey70", alpha = 0.3) +
       geom_line(aes(y = y.exp.lose$predval), col = "blue") + 
       xlab("Change in logged per capita state education revenue") + 
       ylab("Probability of policy adoption") + 
       annotate("text", x = -.225, y = .475, label = "Winners", col = "red") +     	
       annotate("text", x = -.2, y = .4, label = "Losers", col = "blue") + 
       theme(panel.border = element_blank(), panel.background = element_blank()) + 
       xlim(-.26, .23) + 
       scale_y_continuous(oob = rescale_none, limits = c(.25, .75)), 
       file = "fig3a.pdf")
    
p.cheap <- ggplot(data = y.cheap.win, aes(points)) 
ggsave(p.cheap + geom_ribbon(aes(ymin = CI.cheap.win$lower, ymax = CI.cheap.win$upper), fill = "grey70", alpha = 0.3) +
 	   geom_line(aes(y = y.cheap.win$predval), col = "red") + 
 	   geom_ribbon(aes(ymin = CI.cheap.lose$lower, ymax = CI.cheap.lose$upper), fill = "grey70", alpha = 0.3) +
       geom_line(aes(y = y.cheap.lose$predval), col = "blue") + 
   	   xlab("Change in logged per capita state education revenue") + 
       ylab("Probability of policy adoption") + 
       annotate("text", x = -.225, y = .49, label = "Winners", col = "red") +     	
       annotate("text", x = -.2, y = .412, label = "Losers", col = "blue") + 
       theme(panel.border = element_blank(), panel.background = element_blank()) + 
       ylim(.25, .75) + xlim(-.26, .23),
			 file = "fig3b.pdf")

