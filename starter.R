#1
#produce data:
#######animal id (10) 5 in each treatments
#######treatments (A is control, B is treatment of a loss of weight by 30% linear relation)
#######measurements (4 timepoints, weight, r.v)
#average mouse weight = 9.3g - 31.8g, depending on age and gender, given constant nutrition.
#weight of mouse found via website "https://www.jax.org/jax-mice-and-services/strain-data-sheet-pages/body-weight-chart-000664"
#######with this data, we will use weight of males at 6weeks of age, giving us mu = 21.1g and sigma = 1.5, assuming mouse weight has normal dist.

####################################################################################################################################################################################################################################

#data made for control
library(MASS)
library(nlme)
set.seed(615)
n <- 5
beta01 <- 21.1
beta11 <- 0.00

ar.val <- .3

sigma <- 1.5
tau0  <- 0.5
tau1  <- 0.5
tau01 <- 0.5
m <- 5

p <- round(runif(n,4,m))

obs <- unlist(sapply(p, function(x) c(1, sort(sample(2:m, x-1, replace=FALSE)))))

dat1 <- data.frame(id=rep(1:n, times=p), obs=obs)

mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
tau <- c(tau0, tau1)
S   <- diag(tau) %*% S %*% diag(tau)
U   <- mvrnorm(n, mu=mu, Sigma=S)

dat1$eij <- unlist(sapply(p, function(x) arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma))
dat1$yij <- (beta01 + rep(U[,1], times=p)) + (beta11 + rep(U[,2], times=p)) * log(dat1$obs) + dat1$eij

dat1 <- groupedData(yij ~ obs | id, data=dat1)

####################################################################################################################################################################################################################################

#data made for tx
set.seed(327)
n <- 5
beta02 <- 21.1
beta12 <- -3

ar.val <- .3

sigma <- 1.5
tau0  <- 0.5
tau1  <- 0.5
tau01 <- 0.5
m <- 5

p <- round(runif(n,4,m))

obs <- unlist(sapply(p, function(x) c(1, sort(sample(2:m, x-1, replace=FALSE)))))

dat2 <- data.frame(id=rep(1:n, times=p), obs=obs)

mu  <- c(0,0)
S   <- matrix(c(1, tau01, tau01, 1), nrow=2)
tau <- c(tau0, tau1)
S   <- diag(tau) %*% S %*% diag(tau)
U   <- mvrnorm(n, mu=mu, Sigma=S)

dat2$eij <- unlist(sapply(p, function(x) arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma))
dat2$yij <- (beta02 + rep(U[,1], times=p)) + (beta12 + rep(U[,2], times=p)) * log(dat2$obs) + dat2$eij

dat2 <- groupedData(yij ~ obs | id, data=dat2)

####################################################################################################################################################################################################################################

#additional columns for dataframe
mousedata <- rbind(dat1, dat2)
mousedata$tx <- c("A", "A", "A","A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B")
mousedata$mousenumber <- c("1", "1", "1", "1", "1","2", "2", "2", "2", "2", "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "5", "5", "6", "6", "6", "6", "6", "7", "7", "7", "7", "8", "8", "8", "8", "8", "9", "9", "9", "9", "9", "10", "10", "10", "10", "10")
newmousedata <- mousedata[ mousedata$obs != "5", , drop=FALSE]
newmousedata$trial <- c("1", "2", "3", "4", "1","2", "3", "4", "1", "2", "3", "1", "3", "4", "1", "2", "3", "4", "5", "6", "7", "8", "5", "6", "7", "5", "6", "7", "8", "5", "6", "7", "8", "5", "6", "7", "8")
newmousedata

####################################################################################################################################################################################################################################

library(Rmisc)

#table of averages and sd and se
#1-4 are control, 5-8 are tx

table <- summarySE(newmousedata, measurevar="yij", groupvars=c("tx","obs"))

####################################################################################################################################################################################################################################

#additional cleanup
newmousedata$id <- as.factor(newmousedata$id)
newmousedata$obs <- as.factor(newmousedata$obs)
newmousedata$tx <- as.factor(newmousedata$tx)
newmousedata$mousenumber <- as.factor(newmousedata$mousenumber)
newmousedata$trial <- as.factor(newmousedata$trial)
table$obs <- as.factor(table$obs)
table$tx <- as.factor(table$tx)

####################################################################################################################################################################################################################################

library(ggplot2)
library(plotly)
library(rmarkdown)
library(plyr)
library(Hmisc)

#ggplot
graph1 <- ggplot(data = newmousedata, aes(x = obs, y = yij, group = mousenumber))

#individual movement
graphy1 <- graph1 +
  geom_line(aes(colour=tx)) +
  geom_point(aes(colour=tx)) + theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#movement based on means, with sd (normal)
graph2 <- ggplot(data = newmousedata, aes(x = obs, y = yij, group = tx))

#cl normal
graphy2 <- graph2 +
  stat_summary(fun.y = mean, geom = "point", aes(colour=tx)) +
  stat_summary(fun.y = mean, geom = "line", aes(colour=tx)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(color=tx)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#standard error
graphy3 <- graph2 +
  stat_summary(fun.y = mean, geom = "point", aes(colour=tx)) +
  stat_summary(fun.y = mean, geom = "line", aes(colour=tx)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color=tx)) +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#new mean and error graph, using sd and se and ci
graph3 <- ggplot(table, aes(x=obs, y=yij, group = tx, color=tx))

#standard deviation
graphy4 <- graph3 +
  geom_errorbar(aes(ymin = yij - sd, ymax=yij + sd), width=.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#standard error
graphy5 <- graph3 +
  geom_errorbar(aes(ymin = yij - se, ymax=yij + se), width=.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points") + labs(y = "Weight (g)") +
  ggtitle("Changes in Mean Weights of Mice from Control and Treatment Groups") 

#confidence intervals
graphy6 <- graph3 +
  geom_errorbar(aes(ymin = yij - ci, ymax=yij + ci), width=.2, size=0.7, position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line() +
  theme_bw() +
  theme(axis.title.y = element_text(vjust= 1.8), axis.title.x = element_text(vjust= -0.5), axis.title = element_text(face = "bold")) +
  labs(x = "Time Points") + labs(y = "Weight (g)") +
  ggtitle("Changes in Weights of Individual Mice from Control and Treatment Groups") 

#plotly
#individual tx movement
plot1 <- plot_ly(newmousedata, x = ~obs, y = ~yij, type = 'scatter', mode = 'lines', linetype = ~tx, color = I('black'))
plot1

#mean and sd
plot2 <- plot_ly(data = table[which(table$tx == 'A'),], x = ~obs, y = ~yij, type = 'scatter', mode = 'lines+markers', name = 'A', error_y = ~list(value = sd, color = '#000000')) %>% add_trace(data = table[which(table$tx == 'B'),], name = 'B')
plot2

#mean and se
plot3 <- plot_ly(data = table[which(table$tx == 'A'),], x = ~obs, y = ~yij, type = 'scatter', mode = 'lines+markers', name = 'A', error_y = ~list(value = se, color = '#000000')) %>% add_trace(data = table[which(table$tx == 'B'),], name = 'B')
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
