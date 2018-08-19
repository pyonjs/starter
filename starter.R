
#produce data:
#######animal id (10) 5 in each treatments
#######treatments (A is control, B is treatment of a loss of weight by 30% linear relation)
#######measurements (4 timepoints, weight, r.v)
#average mouse weight = 9.3g - 31.8g, depending on age and gender, given constant nutrition.
#weight of mouse found via website "https://www.jax.org/jax-mice-and-services/strain-data-sheet-pages/body-weight-chart-000664"
#######with this data, we will use weight of males at 6weeks of age, giving us mu = 21.1g and sigma = 1.5, assuming mouse weight has normal dist.

####################################################################################################################################################################################################################################

#data generation of mice weights, with normal error. Version3.0

library(MASS)
library(nlme)

#mouse 0 from time 0-3
ma00 <- rnorm(1, mean = 21.1, sd = 1.5)
ma01 <- ma00 + 1.8 + rnorm(1, mean = 0, sd = 1)
ma02 <- ma01 + 1.1 + rnorm(1, mean = 0, sd = 1)
ma03 <- ma02 + 1.0 + rnorm(1, mean = 0, sd = 1)
ma0 <- c(ma00, ma01, ma02, ma03)

#mouse 1 from time 0-3
ma10 <- rnorm(1, mean = 21.1, sd = 1.5)
ma11 <- ma10 + 1.8 + rnorm(1, mean = 0, sd = 1)
ma12 <- ma11 + 1.1 + rnorm(1, mean = 0, sd = 1)
ma13 <- ma12 + 1.0 + rnorm(1, mean = 0, sd = 1)
ma1 <- c(ma10, ma11, ma12, ma13)

#mouse 2 from time 0-3
ma20 <- rnorm(1, mean = 21.1, sd = 1.5)
ma21 <- ma20 + 1.8 + rnorm(1, mean = 0, sd = 1)
ma22 <- ma21 + 1.1 + rnorm(1, mean = 0, sd = 1)
ma23 <- ma22 + 1.0 + rnorm(1, mean = 0, sd = 1)
ma2 <- c(ma20, ma21, ma22, ma23)

#mouse 3 from time 0-3
ma30 <- rnorm(1, mean = 21.1, sd = 1.5)
ma31 <- ma30 + 1.8 + rnorm(1, mean = 0, sd = 1)
ma32 <- ma31 + 1.1 + rnorm(1, mean = 0, sd = 1)
ma33 <- ma32 + 1.0 + rnorm(1, mean = 0, sd = 1)
ma3 <- c(ma30, ma31, ma32, ma33)

#mouse 4 from time 0-3
ma40 <- rnorm(1, mean = 21.1, sd = 1.5)
ma41 <- ma40 + 1.8 + rnorm(1, mean = 0, sd = 1)
ma42 <- ma41 + 1.1 + rnorm(1, mean = 0, sd = 1)
ma43 <- ma42 + 1.0 + rnorm(1, mean = 0, sd = 1)
ma4 <- c(ma40, ma41, ma42, ma43)

#all mice from treatment A
ma <- data.frame(ma0, ma1, ma2, ma3, ma4)
ma <- as.data.frame(t(ma))

#primary cleanup of data
names(ma)[names(ma) == "V1"] <- "t0"
names(ma)[names(ma) == "V2"] <- "t1"
names(ma)[names(ma) == "V3"] <- "t2"
names(ma)[names(ma) == "V4"] <- "t3"

#extra columns for treatment A
data1 <- stack(ma)
txa <- rep("A",length(1:5))
data1 <- data.frame(data1, txa)

data1$id <- rep(c(1:5))

data1$time <- ifelse(data1$ind == "t0", 0,
                     ifelse(data1$ind == "t1", 1,
                            ifelse(data1$ind == "t2", 2,
                                   ifelse(data1$ind == "t3", 3, F))))

names(data1)[names(data1) == "txa"] <- "treatment"
data1$txa <- data1$trt
names(data2)[names(data2) == "txb"] <- "treatment"
data2$txb <- data2$trt

####################################################################################################################################################################################################################################

#mouse 5 from time 0-3
mb50 <- rnorm(1, mean = 21.1, sd = 1.5)
mb51 <- mb50 - 2 + rnorm(1, mean = 0, sd = 1)
mb52 <- mb51 - 2 + rnorm(1, mean = 0, sd = 1)
mb53 <- mb52 - 2 + rnorm(1, mean = 0, sd = 1)
mb5 <- c(mb50, mb51, mb52, mb53)

#mouse 6 from time 0-3
mb60 <- rnorm(1, mean = 21.1, sd = 1.5)
mb61 <- mb60 - 2 + rnorm(1, mean = 0, sd = 1)
mb62 <- mb61 - 2 + rnorm(1, mean = 0, sd = 1)
mb63 <- mb62 - 2 + rnorm(1, mean = 0, sd = 1)
mb6 <- c(mb60, mb61, mb62, mb63)

#mouse 7 from time 0-3
mb70 <- rnorm(1, mean = 21.1, sd = 1.5)
mb71 <- mb70 - 2  + rnorm(1, mean = 0, sd = 1)
mb72 <- mb71 - 2  + rnorm(1, mean = 0, sd = 1)
mb73 <- mb72 - 2  + rnorm(1, mean = 0, sd = 1)
mb7 <- c(mb70, mb71, mb72, mb73)

#mouse 8 from time 0-3
mb80 <- rnorm(1, mean = 21.1, sd = 1.5)
mb81 <- mb80 - 2 + rnorm(1, mean = 0, sd = 1)
mb82 <- mb81 - 2 + rnorm(1, mean = 0, sd = 1)
mb83 <- mb82 - 2 + rnorm(1, mean = 0, sd = 1)
mb8 <- c(mb80, mb81, mb82, mb83)

#mouse 9 from time 0-3
mb90 <- rnorm(1, mean = 21.1, sd = 1.5)
mb91 <- mb90 - 2 + rnorm(1, mean = 0, sd = 1)
mb92 <- mb91 - 2 + rnorm(1, mean = 0, sd = 1)
mb93 <- mb92 - 2 + rnorm(1, mean = 0, sd = 1)
mb9 <- c(mb90, mb91, mb92, mb93)

#all mice from treatment A
mb <- data.frame(mb5, mb6, mb7, mb8, mb9)
mb <- as.data.frame(t(mb))

#primary cleanup of data
names(mb)[names(mb) == "V1"] <- "t0"
names(mb)[names(mb) == "V2"] <- "t1"
names(mb)[names(mb) == "V3"] <- "t2"
names(mb)[names(mb) == "V4"] <- "t3"

#extra columns for treatment B
data2 <- stack(mb)
txb <- rep("B",length(1:5))
data2 <- data.frame(data2, txb)

data2$id <- rep(c(6:10))

data2$time <- ifelse(data2$ind == "t0", 0,
                     ifelse(data2$ind == "t1", 1,
                            ifelse(data2$ind == "t2", 2,
                                   ifelse(data2$ind == "t3", 3, F))))

####################################################################################################################################################################################################################################

#fixing column txa and txa to be same
names(data1)[names(data1) == "txa"] <- "treatment"
data1$txa <- data1$trt
names(data2)[names(data2) == "txb"] <- "treatment"
data2$txb <- data2$trt

#combining data
mousedata <- rbind(data1, data2)
mousedata$ind <- NULL

#renaming columns
names(mousedata)[names(mousedata) == "values"] <- "wjt"
names(mousedata)[names(mousedata) == "treatment"] <- "trt"

####################################################################################################################################################################################################################################

#table of means and errors
library(Rmisc)

table <- summarySE(mousedata, measurevar="wjt", groupvars=c("trt","time"))
table$ci <- NULL

####################################################################################################################################################################################################################################

#factoring columns
mousedata$trt <- as.factor(mousedata$trt)
mousedata$id <- as.factor(mousedata$id)
table$trt <- as.factor(table$trt)

####################################################################################################################################################################################################################################

library(ggplot2)
library(plotly)
library(rmarkdown)
library(plyr)
library(Hmisc)

#ggplot
graph1 <- ggplot(data = mousedata, aes(x = time, y = wjt, group = id))

#individual movement
graphy1 <- graph1 +
  geom_line(aes(colour=trt)) +
  geom_point(aes(colour=trt)) + theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") + ylim(0, 30) +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

####################################################################################################################################################################################################################################

#movement based on means
graph2 <- ggplot(data = mousedata, aes(x = time, y = wjt, group = trt))

#confidence level normal
graphy2 <- graph2 +
  stat_summary(fun.y = mean, geom = "point", aes(colour=trt), position = position_dodge(0.1)) +
  stat_summary(fun.y = mean, geom = "line", aes(colour=trt)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(color=trt), width = 0.2, position = position_dodge(0.1)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") + ylim(0, 30) +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#standard error
graphy3 <- graph2 +
  stat_summary(fun.y = mean, geom = "point", aes(colour=trt), position = position_dodge(0.1)) +
  stat_summary(fun.y = mean, geom = "line", aes(colour=trt)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=trt), width = 0.2, position = position_dodge(0.1)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") + ylim(0, 30) +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups")

####################################################################################################################################################################################################################################

#new mean and error graph, using sd
graph3 <- ggplot(table, aes(x=time, y=wjt, group = trt, color=trt))

#standard deviation
graphy4 <- graph3 +
  geom_errorbar(aes(ymin = wjt - sd, ymax = wjt + sd), width=0.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") + ylim(0, 30) +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

####################################################################################################################################################################################################################################

#plotly
#individual tx movement
plot1 <- plot_ly(mousedata, x = ~time, y = ~wjt, type = 'scatter', mode = 'lines', linetype = ~id, color = I('black'))
plot1

#mean and sd
plot2 <- plot_ly(data = table[which(table$trt == 'A'),], x = ~time, y = ~wjt, type = 'scatter', mode = 'lines+markers', name = 'A', error_y = ~list(value = sd, color = '#000000')) %>% add_trace(data = table[which(table$trt == 'B'),], name = 'B')
plot2

#mean and se
plot3 <- plot_ly(data = table[which(table$trt == 'A'),], x = ~time, y = ~wjt, type = 'scatter', mode = 'lines+markers', name = 'A', error_y = ~list(value = se, color = '#000000')) %>% add_trace(data = table[which(table$trt == 'B'),], name = 'B')
plot3

#plot2 and plot3 are a problem at the moment. both are similar, except for the change from sd to se between plot2 and plot3,
#but for some reason they are the same plot when error bars should be contrained in plot3. i will try to fix this but i have
#no idea at the moment on how to fix this.

####################################################################################################################################################################################################################################

#analysis
library(car)
library(MASS)
library(lme4)

#lme model for individual mice
lmetest1 <- lmer(wjt ~ time + (time | id), data = mousedata)

#sd of intercept and slope is 0.2517g and 1.7749g/week
#beta of intercept and slope is 20.665070g and -0.009812g/week

#lme model for treatment groups
lmetest2 <- lmer(wjt ~ time + (time | trt), data = mousedata)

#sd of intercept and slope is 0.3351g and 2.2849g/week
#beta of intercept and slope is 20.665070g and -0.009812g/week

#lme model for individual mice with no correlation
lmetest3 <- lmer(wjt ~ time + (time || id), data = mousedata)

#sd of intercept and slope is 1.2553g and 1.7791g/week
#beta of intercept and slope is 20.665070g and -0.009812g/week





















####################################################################################################################################################################################################################################

#save
save.image("C:/Users/PyonJ/OneDrive/Documents/starter/starter.RData")
