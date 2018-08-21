x <- rep(1:4, 10)
trt <- rep(0:1, each=20)
id <- rep(1:10, each=4)
y <- 21.1 - 2*x*trt - x*(trt-1) + rnorm(length(x), mean = 0, sd = 1.5)
trt <- factor(trt)


