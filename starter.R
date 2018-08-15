
#produce data:
#######animal id (10) 5 in each treatments
#######treatments (A is control, B is treatment of a loss of weight by 30% linear relation)
#######measurements (4 timepoints, weight, r.v)
#average mouse weight = 9.3g - 31.8g, depending on age and gender, given constant nutrition.
#weight of mouse found via website "https://www.jax.org/jax-mice-and-services/strain-data-sheet-pages/body-weight-chart-000664"
#######with this data, we will use weight of males at 6weeks of age, giving us mu = 21.1g and sigma = 1.5, assuming mouse weight has normal dist.

####################################################################################################################################################################################################################################

#data generation of mice weights, with normal error. Version2.0

library(MASS)
library(nlme)

#generation of control treatment

ta0 <- rnorm(5, mean = 21.1, sd = 1.5)
ta1 <- ta0 + 1.8 + rnorm(1, mean = 0, sd = 1)
ta2 <- ta1 + 1.1 + rnorm(1, mean = 0, sd = 1)
ta3 <- ta2 + 1.0 + rnorm(1, mean = 0, sd = 1)
data1 <- data.frame(ta0, ta1, ta2, ta3)
data1 <- stack(data1)
txa <- rep("A",length(1:5))
data1 <- data.frame(data1, txa)

data1$id <- rep(c(1:5))

data1$time <- ifelse(data1$ind == "ta0", 0,
                     ifelse(data1$ind == "ta1", 1,
                            ifelse(data1$ind == "ta2", 2,
                                   ifelse(data1$ind == "ta3", 3, F))))

names(data1)[names(data1) == "values"] <- "weight"
names(data1)[names(data1) == "txa"] <- "treatment"

data1$values <- data1$wjt
data1$txa <- data1$trt

#generation of weight loss treatment

tb0 <- rnorm(5, mean = 21.1, sd = 1.5)
tb1 <- tb0 - 2 + 1.8 + rnorm(1, mean = 0, sd = 1)
tb2 <- tb1 - 2 + 1.8 + rnorm(1, mean = 0, sd = 1)
tb3 <- tb2 - 2 + 1.8 + rnorm(1, mean = 0, sd = 1)
txb <- rep("B",length(1:5))
data2 <- data.frame(tb0, tb1, tb2, tb3)
data2 <- stack(data2)
txb <- rep("B",length(1:5))
data2 <- data.frame(data2, txb)

data2$id <- rep(c(6:10))

data2$time <- ifelse(data2$ind == "tb0", 0,
                     ifelse(data2$ind == "tb1", 1,
                            ifelse(data2$ind == "tb2", 2,
                                   ifelse(data2$ind == "tb3", 3, F))))

names(data2)[names(data2) == "values"] <- "weight"
names(data2)[names(data2) == "txb"] <- "treatment"

data2$values <- data2$wjt
data2$txb <- data2$trt

mousedata <- rbind(data1, data2)
mousedata$ind <- NULL

# At the moment, I am not sure how to fix how the data is generated. The data seems to be generated
#with the rnorm() part having a set seed for every initiation of the function. The intention was 
#for the rnorm part to produce a random number each time the function is applied to each individual
#mouse. It is difficult to find the proper coding at the moment.

####################################################################################################################################################################################################################################

library(Rmisc)

table <- summarySE(mousedata, measurevar="weight", groupvars=c("treatment","time"))

####################################################################################################################################################################################################################################

#additional cleanup

mousedata$treatment <- as.factor(mousedata$treatment)
mousedata$id <- as.factor(mousedata$id)
table$treatment <- as.factor(table$treatment)

####################################################################################################################################################################################################################################

####################################################################################################################################################################################################################################

####################################################################################################################################################################################################################################

library(ggplot2)
library(plotly)
library(rmarkdown)
library(plyr)
library(Hmisc)

#ggplot
graph1 <- ggplot(data = mousedata, aes(x = time, y = weight, group = id))

#individual movement
graphy1 <- graph1 +
  geom_line(aes(colour=treatment)) +
  geom_point(aes(colour=treatment)) + theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#movement based on means, with sd (normal)
graph2 <- ggplot(data = mousedata, aes(x = time, y = weight, group = treatment))

#cl normal
graphy2 <- graph2 +
  stat_summary(fun.y = mean, geom = "point", aes(colour=treatment)) +
  stat_summary(fun.y = mean, geom = "line", aes(colour=treatment)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(color=tx)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#standard error
graphy3 <- graph2 +
  stat_summary(fun.y = mean, geom = "point", aes(colour=treatment)) +
  stat_summary(fun.y = mean, geom = "line", aes(colour=treatment)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=treatment)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#new mean and error graph, using sd and se and ci
graph3 <- ggplot(table, aes(x=time, y=weight, group = treatment, color=treatment))

#standard deviation
graphy4 <- graph3 +
  geom_errorbar(aes(ymin = weight - sd, ymax = weight + sd), width=.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#standard error
graphy5 <- graph3 +
  geom_errorbar(aes(ymin = weight - se, ymax = weight + se), width=.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") +
  ggtitle("Changes in Mean Weights of Mice from Control and Treatment Groups") 

#confidence intervals
graphy6 <- graph3 +
  geom_errorbar(aes(ymin = weight - ci, ymax = weight + ci), width=.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points (1 wk)") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#plotly
#individual tx movement
plot1 <- plot_ly(mousedata, x = ~time, y = ~weight, type = 'scatter', mode = 'lines', linetype = ~treatment, color = I('black'))
plot1

#mean and sd
plot2 <- plot_ly(data = table[which(table$treatment == 'A'),], x = ~time, y = ~weight, type = 'scatter', mode = 'lines+markers', name = 'A', error_y = ~list(value = sd, color = '#000000')) %>% add_trace(data = table[which(table$treatment == 'B'),], name = 'B')
plot2

#mean and se
plot3 <- plot_ly(data = table[which(table$treatment == 'A'),], x = ~time, y = ~weight, type = 'scatter', mode = 'lines+markers', name = 'A', error_y = ~list(value = se, color = '#000000')) %>% add_trace(data = table[which(table$treatment == 'B'),], name = 'B')
plot3

#plot2 and plot3 are a problem at the moment. both are similar, except for the change from sd to se between plot2 and plot3,
#but for some reason they are the same plot when error bars should be contrained in plot3. i will try to fix this but i have
#no idea at the moment on how to fix this.


####################################################################################################################################################################################################################################

#analysis
library(car)
library(MASS)

#check for normal
normalcheckdata <- subset(newmousedata, tx=="A")
model <- lm(yij ~ obs, data = normalcheckdata)
qqPlot(model, distribution="norm", main="QQ Plot")

#t-test
tempanova1 <- aov(yij ~ obs:tx, data = newmousedata)
TukeyHSD(tempanova2, conf.level = 0.95)

#pvalue of 1A-1B is 0.9996832, 2A-2B is 0.9924810, 3A-3B is 0.1371564, and 4A-4b is 0.0352388

#
model1 <- lm(yij ~ obs + tx + obs:tx, data = newmousedata)
Anova(model1, type = "II")
summary(model1)





























#save
save.image("C:/Users/PyonJ/OneDrive/Documents/starter/starter.RData")
