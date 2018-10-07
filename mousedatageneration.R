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
time2 <- rep(0:3, 100)
trt2 <- rep(0:1, each=200)
id2 <- rep(1:100, each=4)

wjt2 <- 24.7170 - 0.5328*time2 - 7.1457*trt2 + rnorm(length(time2), mean = 0, sd = 1.5)
dt2 <- data.frame(time2, trt2, wjt2, id2)
t2 <- summarySE(dt2, measurevar="wjt2", groupvars=c("trt2","time2"), conf.interval = 0.95)
pl3 <- ggplot(data = dt2, aes(x = time2, y = wjt2, group = id2, color = trt2)) + geom_line() + geom_point()
pl4 <- ggplot(data = t2, aes(x = time2, y = wjt2, group = trt2, color = trt2)) + geom_errorbar(aes(ymin = wjt2 - ci, ymax = wjt2 + ci)) + geom_line() + geom_point()



mod2 <- lm(wjt ~ time + trt + time*trt, data = dt1)
summary(mod2)
plot(mod2)

#y = 22.1961 + 0.5203x1 - 3.0872x2 - 1.9098x1x2 + err

wjt3 <- 22.1961 + 0.5203*time2 - 1.9098*time2*trt2 + rnorm(length(time2), mean = 0, sd = 1.5)
trt2 <- factor(trt2)
dt3 <- data.frame(time2, trt2, wjt3, id2)
t3 <- summarySE(dt3, measurevar="wjt3", groupvars=c("trt2","time2"), conf.interval = 0.95)
pl5 <- ggplot(data = dt3, aes(x = time2, y = wjt3, group = id2, color = trt2)) + geom_line() + geom_point()
pl6 <- ggplot(data = t3, aes(x = time2, y = wjt3, group = trt2, color = trt2)) + geom_errorbar(aes(ymin = wjt3 - ci, ymax = wjt3 + ci)) + geom_line() + geom_point()

mod3 <- lm(wjt3 ~ time2 + trt2 + time2*trt2)
summary(mod3)

anova(mod1, mod2)

####################################################################################################################################################################################################################################

















