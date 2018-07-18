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
beta11 <- 0.01

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

library(ggplot2)
library(Hmisc)
graph1 <- ggplot(data = newmousedata, aes(x = obs, y = yij, group = mousenumber))

#spaghetti graph
graph1 + geom_line(aes(colour=tx))
#line graph
graph1 + geom_point(aes(colour=tx))

graph2 <- ggplot(data = newmousedata, aes(x = obs, y = yij, group = tx))
graph2 + stat_summary(fun.y = mean, geom = "point", aes(colour=tx)) + stat_summary(fun.y = mean, geom = "line", aes(colour=tx)) + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(colour=tx))

####################################################################################################################################################################################################################################

#table of averages and sd
#1-4 are control, 5-8 are tx
table <- aggregate( yij ~ trial, FUN=function(x) c(mn=mean(x), sd=sd(x)), data=newmousedata)



















