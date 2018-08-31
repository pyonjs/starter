library(MASS)

time<- rep(1:4, 10)
trt <- rep(0:1, each=20)
id <- rep(1:10, each=4)
wjt <- 21.1 - 2*time*trt - time*(trt-1) + rnorm(length(time), mean = 0, sd = 1.5)
trt <- factor(trt)
id <- factor(id)

# y = 21.1 + time - 3*trt*time + err


####################################################################################################################################################################################################################################

library(Rmisc)
library(ggplot2)

dt1 <- data.frame(time, trt, wjt, id)
t1 <- summarySE(dt1, measurevar="wjt", groupvars=c("trt","time"), conf.interval = 0.95)
pl1 <- ggplot(data = dt1, aes(x = time, y = wjt, group = id, color = trt)) + geom_line() + geom_point()
pl2 <- ggplot(data = t1, aes(x = time, y = wjt, group = trt, color = trt)) + geom_errorbar(aes(ymin = wjt - ci, ymax = wjt + ci)) + geom_line() + geom_point()

####################################################################################################################################################################################################################################

mod1 <- lm(wjt ~ time + trt, data = dt1)
summary(mod1)
plot(mod1)


#y = 24.7170 - 0.5328x1 - 7.1457x2 + err

mod2 <- lm(wjt ~ time + trt + time*trt, data = dt1)
summary(mod2)
plot(mod2)

#y = 22.1961 + 0.5203x1 - 3.0872x2 - 1.9098x1x2 + err

mod3 <- lm(wjt ~ time + time*trt, data = dt1)
summary(mod3)
plot(mod3)

anova(mod1, mod2)

####################################################################################################################################################################################################################################

aov(wjt ~ trt)
TukeyHSD(aov(wjt ~ trt), conf.level =  0.95)

aov(wjt ~ trt + id)
TukeyHSD(aov(wjt ~ trt + id), conf.level =  0.95)


plot(wjt ~ trt + id)

anova(mod1, mod2)

####################################################################################################################################################################################################################################


















