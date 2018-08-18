#data generation of mice weights, with normal error. Version3.0

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

#table of means and errors
library(Rmisc)

table <- summarySE(mousedata, measurevar="wjt", groupvars=c("trt","time"))

#factoring columns
mousedata$trt <- as.factor(mousedata$trt)
mousedata$id <- as.factor(mousedata$id)
table$trt <- as.factor(table$trt)








