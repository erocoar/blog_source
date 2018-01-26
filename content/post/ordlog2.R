wines <- read.csv("wines.csv", sep = "")




                         




##### ----
##### ---- separation plot 
##### ----
setwd("D:/onedrive/github/blog_source/content/posts")
library(foreign)
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
m <- polr(apply ~ pared + public + gpa, data = dat)

fitted <- m$fitted.values

prob1 <- 1 / (1 + exp(-(m$zeta[1] - coef(m)[1]*dat$pared - coef(m)[2]*dat$public - coef(m)[3]*dat$gpa)))
prob2 <- 1 / (1 + exp(-(m$zeta[2] - coef(m)[1]*dat$pared - coef(m)[2]*dat$public - coef(m)[3]*dat$gpa)))
prob3 <- 1 / (1 + exp(-(m$zeta[3] - coef(m)[1]*dat$pared - coef(m)[2]*dat$public - coef(m)[3]*dat$gpa)))
test2 <- cbind(prob1, prob2-prob1, 1-prob2)

yevent <- cbind(as.numeric(dat$apply) <= 1, as.numeric(dat$apply) <= 2)

gq1 <- cbind(fitted[, 1], yevent[, 1])
gq1 <- gq1[order(gq1[, 1]), ]

gq2 <- cbind(fitted[, 2], yevent[, 2])
gq2 <- gq2[order(gq2[, 1]), ]
#



summary(t1)
summary(m)

ggplot() + theme(legend.position = "none") +
  geom_linerange(aes(ymin = 0, ymax = 1, x = 1:nrow(gq1), colour = factor(gq1[, 2]))) +
  scale_color_manual(values = col) +
  geom_line(aes(y = gq1[, 1], x = 1:nrow(gq1)), lwd = 0.8)

ggplot() + theme(legend.position = "none") +
  geom_linerange(aes(ymin = 0, ymax = 1, x = 1:nrow(gq2), colour = factor(gq2[, 2]))) +
  scale_color_manual(values = col) +
  geom_line(aes(y = gq2[, 1], x = 1:nrow(gq2)), lwd = 0.8)



sum(gq2[, 2])




test2 <- m1$fitted.values

test1 <- wines$price * m1$coefficients + m1$zeta[1]

test2[1:5]
(wines$price * coef(m1) + m1$zeta)[1:5]

prob1 <- 1 / (1 + exp(-(m1$zeta[1] - coef(m1) * wines$price)))
prob2 <- 1 / (1 + exp(-(m1$zeta[2] - coef(m1) * wines$price))) 
prob3 <- 1 / (1 + exp(-(m1$zeta[3] - coef(m1) * wines$price)))
test2 <- cbind(prob1, prob2-prob1, prob3-prob2, 1-prob3)

y_events <- cbind(wines$points >= 1, wines$points >= 2, wines$points >= 3)

library(ggplot2)

gq1 <- cbind(prob1, wines$points >= 1, wines$points)
gq1 <- gq1[order(gq1[, 1]), ]

gq2 <- cbind(test2[, 2], y_events[, 2])
gq2 <- gq2[order(gq2[, 1]), ]

gq3 <- cbind(test2[, 3], y_events[, 3])
gq3 <- gq2[order(gq3[, 1]), ]

col<-c(rgb(red = 254, green = 232, blue = 200, max = 255), 
       rgb(red = 227, green = 74, blue = 51, max = 255))  

ggplot() + 
  geom_linerange(aes(ymin = 0, ymax = 1, x = 1:nrow(gq1), colour = factor(gq1[, 2]))) +
  scale_color_manual(values = col) +
  geom_line(aes(y = gq1[, 1], x = 1:nrow(gq1)), lwd = 0.8)


ggplot() + 
  geom_linerange(aes(ymin = 0, ymax = 1, x = 1:nrow(gq2), colour = factor(gq2[, 2]))) +
  scale_color_manual(values = col) +
  geom_line(aes(y = gq2[, 1], x = 1:nrow(gq2)), lwd = 0.8)


ggplot() + 
  geom_linerange(aes(ymin = 0, ymax = 1, x = 1:nrow(gq3), colour = factor(gq3[, 2]))) +
  scale_color_manual(values = col) +
  geom_line(aes(y = gq3[, 1], x = 1:nrow(gq3)), lwd = 0.8)


