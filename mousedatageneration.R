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


